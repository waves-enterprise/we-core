package com.wavesenterprise.transaction

import com.wavesenterprise.account.{AddressScheme, PublicKeyAccount}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.assets.{IssueTransactionV3, ReissueTransactionV3}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalacheck.Gen
import play.api.libs.json._

class ReissueTransactionV3Specification extends GenericTransactionSpecification[ReissueTransactionV3] {

  def transactionParser: com.wavesenterprise.transaction.TransactionParserFor[ReissueTransactionV3] = ReissueTransactionV3

  def updateProofs(tx: ReissueTransactionV3, p: Proofs): ReissueTransactionV3 = {
    tx.copy(proofs = p)
  }

  def assertTxs(first: ReissueTransactionV3, second: ReissueTransactionV3): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.quantity shouldEqual second.quantity
    first.reissuable shouldEqual second.reissuable
    first.assetId shouldEqual second.assetId
    first.atomicBadge shouldEqual second.atomicBadge
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  def generator: Gen[(Seq[com.wavesenterprise.transaction.Transaction], ReissueTransactionV3)] =
    for {
      (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      fee                                                                      <- smallFeeGen
      reissuable                                                               <- Gen.oneOf(true, false)
      atomicBadgeOpt                                                           <- atomicBadgeOptGen
    } yield {
      val issue = IssueTransactionV3
        .selfSigned(currentChainId, sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp, atomicBadgeOpt, None)
        .explicitGet()
      val reissue1 = ReissueTransactionV3
        .selfSigned(AddressScheme.getAddressSchema.chainId,
                    sender,
                    issue.assetId(),
                    quantity,
                    reissuable = reissuable,
                    fee,
                    timestamp,
                    atomicBadgeOpt)
        .explicitGet()
      (Seq(issue), reissue1)
    }

  def jsonRepr: Seq[(JsValue, ReissueTransactionV3)] = {
    val tx = ReissueTransactionV3
      .signed(
        senderAccount,
        'T',
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        100000000L,
        true,
        100000000L,
        1526287561757L,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
      )
      .right
      .get
    val proofs = tx.proofs.base58().head
    Seq(
      (Json.parse(s"""{
      "type": 5,
      "id": "${tx.id()}",
      "sender": "${senderAccount.address}",
      "senderPublicKey": "$senderPkBase58",
      "fee": 100000000,
      "timestamp": 1526287561757,
      "atomicBadge": { "trustedSender": "${senderAccount.address}"},
      "proofs": [
          "$proofs"
      ],
      "version": 3,
      "chainId": 84,
      "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
      "quantity": 100000000,
      "reissuable": true
      }"""),
       tx))
  }

  property(s"$transactionName proto serialization roundtrip") {
    forAll(generator) {
      case (_, tx) =>
        val recovered = ReissueTransactionV3.fromProto(tx.toInnerProto).explicitGet()
        recovered shouldEqual tx
    }
  }

  def transactionName: String = "ReissueTransactionV3"
}
