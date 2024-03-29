package com.wavesenterprise.lang.v1.evaluator.ctx.impl.waves

import com.wavesenterprise.lang.v1.compiler.Terms._
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.converters
import com.wavesenterprise.lang.v1.traits.domain.Tx._
import com.wavesenterprise.lang.v1.traits.domain._
import scodec.bits.ByteVector

object Bindings {

  import converters._

  private def combine(m0: Map[String, EVALUATED], m1: Map[String, EVALUATED]*) = m1.toList.fold(m0)(_ ++ _)

  import Types._

  private def headerPart(tx: Header): Map[String, EVALUATED] = Map(
    "id"        -> tx.id,
    "fee"       -> tx.fee,
    "timestamp" -> tx.timestamp,
    "version"   -> tx.version,
  )

  private def proofsPart(existingProofs: IndexedSeq[ByteVector]) =
    "proofs" -> ARR((existingProofs ++ Seq.fill(8 - existingProofs.size)(ByteVector.empty)).map(CONST_BYTEVECTOR).toIndexedSeq)

  private def provenTxPart(tx: Proven, proofsEnabled: Boolean): Map[String, EVALUATED] = {
    val commonPart = combine(Map(
                               "sender"          -> senderObject(tx.sender),
                               "senderPublicKey" -> tx.senderPk,
                               "bodyBytes"       -> tx.bodyBytes
                             ),
                             headerPart(tx.h))

    if (proofsEnabled) combine(commonPart, Map(proofsPart(tx.proofs)))
    else commonPart
  }
  private def mapRecipient(r: Recipient) =
    "recipient" -> (r match {
      case Recipient.Alias(name) => CaseObj(aliasType.typeRef, Map("alias" -> name))
      case x: Recipient.Address  => senderObject(x)
    })

  def assetPair(ap: APair): CaseObj =
    CaseObj(
      assetPairType.typeRef,
      Map(
        "amountAsset" -> fromOptionBV(ap.amountAsset),
        "priceAsset"  -> ap.priceAsset
      )
    )

  def ordType(o: OrdType): CaseObj =
    CaseObj((o match {
              case OrdType.Buy  => buyType
              case OrdType.Sell => sellType
            }).typeRef,
            Map.empty)

  def orderObject(ord: Ord, proofsEnabled: Boolean): CaseObj =
    CaseObj(
      buildOrderType(proofsEnabled).typeRef,
      Map(
        "id"               -> ord.id,
        "sender"           -> senderObject(ord.sender),
        "senderPublicKey"  -> ord.senderPublicKey,
        "matcherPublicKey" -> ord.matcherPublicKey,
        "assetPair"        -> assetPair(ord.assetPair),
        "orderType"        -> ordType(ord.orderType),
        "amount"           -> ord.amount,
        "price"            -> ord.price,
        "timestamp"        -> ord.timestamp,
        "expiration"       -> ord.expiration,
        "matcherFee"       -> ord.matcherFee,
        "bodyBytes"        -> ord.bodyBytes,
        proofsPart(ord.proofs)
      )
    )

  def senderObject(sender: Recipient.Address): CaseObj = CaseObj(addressType.typeRef, Map("bytes" -> sender.bytes))

  def transactionObject(tx: Tx, proofsEnabled: Boolean): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> CONST_LONG(amount)) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Transfer(p, feeAssetId, assetId, amount, recipient, attachment) =>
        CaseObj(
          buildTransferTransactionType(proofsEnabled).typeRef,
          combine(
            Map(
              "amount"     -> amount,
              "feeAssetId" -> feeAssetId,
              "assetId"    -> assetId,
              "attachment" -> attachment
            ),
            provenTxPart(p, proofsEnabled) + mapRecipient(recipient)
          )
        )
      case Issue(p, quantity, name, description, reissuable, decimals, scriptOpt) =>
        CaseObj(
          buildIssueTransactionType(proofsEnabled).typeRef,
          combine(
            Map(
              "quantity"    -> quantity,
              "name"        -> name,
              "description" -> description,
              "reissuable"  -> reissuable,
              "decimals"    -> decimals,
              "script"      -> scriptOpt
            ),
            provenTxPart(p, proofsEnabled)
          )
        )
      case ReIssue(p, quantity, assetId, reissuable) =>
        CaseObj(
          buildReissueTransactionType(proofsEnabled).typeRef,
          combine(Map(
                    "quantity"   -> quantity,
                    "assetId"    -> assetId,
                    "reissuable" -> reissuable,
                  ),
                  provenTxPart(p, proofsEnabled))
        )
      case Burn(p, quantity, assetId) =>
        CaseObj(
          buildBurnTransactionType(proofsEnabled).typeRef,
          combine(Map(
                    "quantity" -> quantity,
                    "assetId"  -> assetId
                  ),
                  provenTxPart(p, proofsEnabled))
        )
      case Lease(p, amount, recipient) =>
        CaseObj(
          buildLeaseTransactionType(proofsEnabled).typeRef,
          combine(Map("amount" -> amount), provenTxPart(p, proofsEnabled) + mapRecipient(recipient))
        )
      case LeaseCancel(p, leaseId) =>
        CaseObj(
          buildLeaseCancelTransactionType(proofsEnabled).typeRef,
          combine(Map(
                    "leaseId" -> leaseId,
                  ),
                  provenTxPart(p, proofsEnabled))
        )
      case CreateAlias(p, alias) =>
        CaseObj(buildCreateAliasTransactionType(proofsEnabled).typeRef,
                combine(Map(
                          "alias" -> alias,
                        ),
                        provenTxPart(p, proofsEnabled)))
      case MassTransfer(p, assetId, transferCount, totalAmount, transfers, attachment) =>
        CaseObj(
          buildMassTransferTransactionType(proofsEnabled).typeRef,
          combine(
            Map(
              "transfers" -> transfers
                .map(bv => CaseObj(transfer.typeRef, Map(mapRecipient(bv.recipient), "amount" -> bv.amount))),
              "assetId"       -> assetId,
              "transferCount" -> transferCount,
              "totalAmount"   -> totalAmount,
              "attachment"    -> attachment
            ),
            provenTxPart(p, proofsEnabled)
          )
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(buildSetScriptTransactionType(proofsEnabled).typeRef, Map("script" -> fromOptionBV(scriptOpt)) ++ provenTxPart(p, proofsEnabled))
      case SetAssetScript(p, assetId, scriptOpt) =>
        CaseObj(
          buildSetAssetScriptTransactionType(proofsEnabled).typeRef,
          combine(Map("script" -> fromOptionBV(scriptOpt), "assetId" -> assetId), provenTxPart(p, proofsEnabled))
        )
      case Sponsorship(p, assetId, isEnabled) =>
        CaseObj(
          buildSponsorFeeTransactionType(proofsEnabled).typeRef,
          combine(Map("assetId" -> assetId, "isEnabled" -> isEnabled), provenTxPart(p, proofsEnabled))
        )
      case Data(p, data) =>
        def mapValue(e: Any): EVALUATED = e match {
          case s: String     => c(s)
          case s: Boolean    => c(s)
          case s: Long       => c(s)
          case s: ByteVector => c(s)
          case _             => ???
        }

        CaseObj(
          buildDataTransactionType(proofsEnabled).typeRef,
          combine(Map("data" -> data.map(e => CaseObj(dataEntryType.typeRef, Map("key" -> CONST_STRING(e.key), "value" -> mapValue(e.value))))),
                  provenTxPart(p, proofsEnabled))
        )
      case Exchange(p, amount, price, buyMatcherFee, sellMatcherFee, buyOrder, sellOrder) =>
        CaseObj(
          buildExchangeTransactionType(proofsEnabled).typeRef,
          combine(
            Map(
              "buyOrder"       -> orderObject(buyOrder, proofsEnabled),
              "sellOrder"      -> orderObject(sellOrder, proofsEnabled),
              "amount"         -> amount,
              "price"          -> price,
              "buyMatcherFee"  -> buyMatcherFee,
              "sellMatcherFee" -> sellMatcherFee,
            ),
            provenTxPart(p, proofsEnabled)
          )
        )
      case CreateContract(p) =>
        CaseObj(
          buildCreateContractTransactionType(proofsEnabled).typeRef,
          provenTxPart(p, proofsEnabled)
        )
      case CallContract(p, contractId, contractVersion) =>
        CaseObj(
          buildCallContractTransactionType(proofsEnabled).typeRef,
          combine(Map("contractId" -> contractId, "contractVersion" -> contractVersion), provenTxPart(p, proofsEnabled))
        )
      case DisableContract(p, contractId) =>
        CaseObj(
          buildDisableContractTransactionType(proofsEnabled).typeRef,
          combine(Map("contractId" -> contractId), provenTxPart(p, proofsEnabled))
        )
      case UpdateContract(p, contractId) =>
        CaseObj(
          buildUpdateContractTransactionType(proofsEnabled).typeRef,
          combine(Map("contractId" -> contractId), provenTxPart(p, proofsEnabled))
        )
    }

}
