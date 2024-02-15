package com.wavesenterprise.transaction.smart

import com.wavesenterprise.account.{Address, AddressOrAlias, Alias}
import com.wavesenterprise.lang.v1.traits.domain.Tx.{Header, Proven}
import com.wavesenterprise.lang.v1.traits.domain._
import com.wavesenterprise.state._
import com.wavesenterprise.transaction._
import com.wavesenterprise.transaction.assets._
import com.wavesenterprise.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesenterprise.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesenterprise.transaction.docker._
import com.wavesenterprise.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesenterprise.transaction.transfer._
import scodec.bits.ByteVector

object RealTransactionWrapper {

  private def header(tx: Transaction): Header = {
    val v = tx match {
      case vt: VersionedTransaction => vt.version
      case _                        => 1
    }
    Header(ByteVector(tx.id().arr), tx.fee, tx.timestamp, v)
  }
  private def proven(tx: ProvenTransaction): Proven =
    Proven(
      header(tx),
      Recipient.Address(ByteVector(tx.sender.toAddress.bytes.arr)),
      ByteVector(tx.bodyBytes()),
      ByteVector(tx.sender.publicKey.getEncoded),
      tx.proofs.proofs.map(_.arr).map(ByteVector(_)).toIndexedSeq
    )

  implicit def toByteVector(s: ByteStr): ByteVector = ByteVector(s.arr)

  implicit def assetPair(a: AssetPair): APair = APair(a.amountAsset.map(toByteVector), a.priceAsset.map(toByteVector))
  implicit def ord(o: Order): Ord =
    Ord(
      id = ByteVector(o.id.value.arr),
      sender = Recipient.Address(ByteVector(o.sender.toAddress.bytes.arr)),
      senderPublicKey = ByteVector(o.senderPublicKey.publicKey.getEncoded),
      matcherPublicKey = ByteVector(o.matcherPublicKey.publicKey.getEncoded),
      assetPair = o.assetPair,
      orderType = o.orderType match {
        case BUY  => OrdType.Buy
        case SELL => OrdType.Sell
      },
      amount = o.amount,
      price = o.price,
      timestamp = o.timestamp,
      expiration = o.expiration,
      matcherFee = o.matcherFee,
      bodyBytes = ByteVector(o.bodyBytes()),
      proofs = o.proofs.proofs.map(a => ByteVector(a.arr)).toIndexedSeq
    )

  implicit def aoaToRecipient(aoa: AddressOrAlias): Recipient = aoa match {
    case a: Address => Recipient.Address(ByteVector(a.bytes.arr))
    case a: Alias   => Recipient.Alias(a.name)
  }

  def apply(tx: Transaction): Tx = {
    tx match {
      case g: GenesisTransaction => Tx.Genesis(header(g), g.amount, g.recipient)
      case t: TransferTransaction =>
        Tx.Transfer(
          proven(t),
          feeAssetId = t.feeAssetId.map(a => ByteVector(a.arr)),
          assetId = t.assetId.map(a => ByteVector(a.arr)),
          amount = t.amount,
          recipient = t.recipient,
          attachment = ByteVector(t.attachment)
        )
      case i: IssueTransaction =>
        Tx.Issue(proven(i),
                 i.quantity,
                 ByteVector(i.name),
                 ByteVector(i.description),
                 i.reissuable,
                 i.decimals,
                 i.script.map(_.bytes()).map(toByteVector))
      case r: ReissueTransaction     => Tx.ReIssue(proven(r), r.quantity, r.assetId, r.reissuable)
      case b: BurnTransaction        => Tx.Burn(proven(b), b.amount, b.assetId)
      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount, b.recipient)
      case b: LeaseCancelTransaction => Tx.LeaseCancel(proven(b), b.leaseId)
      case b: CreateAliasTransaction => Tx.CreateAlias(proven(b), b.alias.name)
      case ms: MassTransferTransactionV1 =>
        Tx.MassTransfer(
          proven(ms),
          assetId = ms.assetId.map(a => ByteVector(a.arr)),
          transferCount = ms.transfers.length,
          totalAmount = ms.transfers.map(_.amount).sum,
          transfers = ms.transfers.map(r => com.wavesenterprise.lang.v1.traits.domain.Tx.TransferItem(r.recipient, r.amount)).toIndexedSeq,
          attachment = ByteVector(ms.attachment)
        )
      case ss: SetScriptTransaction        => Tx.SetScript(proven(ss), ss.script.map(_.bytes()).map(toByteVector))
      case ss: SetAssetScriptTransactionV1 => Tx.SetAssetScript(proven(ss), ss.assetId, ss.script.map(_.bytes()).map(toByteVector))
      case e: ExchangeTransaction          => Tx.Exchange(proven(e), e.amount, e.price, e.buyMatcherFee, e.sellMatcherFee, e.buyOrder, e.sellOrder)
      case s: SponsorFeeTransactionV1      => Tx.Sponsorship(proven(s), s.assetId, s.isEnabled)
      case d: DataTransaction =>
        Tx.Data(
          proven(d),
          d.data.map {
            case IntegerDataEntry(key, value) => DataItem.Lng(key, value)
            case StringDataEntry(key, value)  => DataItem.Str(key, value)
            case BooleanDataEntry(key, value) => DataItem.Bool(key, value)
            case BinaryDataEntry(key, value)  => DataItem.Bin(key, value)
          }.toIndexedSeq
        )
      case c: CreateContractTransaction  => Tx.CreateContract(proven(c))
      case c: CallContractTransaction    => Tx.CallContract(proven(c), c.contractId, c.contractVersion)
      case d: DisableContractTransaction => Tx.DisableContract(proven(d), d.contractId)
      case u: UpdateContractTransaction  => Tx.UpdateContract(proven(u), u.contractId)
    }
  }
}
