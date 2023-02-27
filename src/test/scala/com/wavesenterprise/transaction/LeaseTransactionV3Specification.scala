package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.{Address, PublicKeyAccount}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.lease.LeaseTransactionV3
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class LeaseTransactionV3Specification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen
    with WithSenderAndRecipient {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseV3Gen) { tx: LeaseTransactionV3 =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get.asInstanceOf[LeaseTransactionV3]
      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction proto serialization roundtrip") {
    forAll(leaseV3Gen) { tx =>
      val recovered = LeaseTransactionV3.fromProto(tx.toInnerProto).explicitGet()
      tx shouldEqual recovered
    }
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseV3Gen) { tx: LeaseTransactionV3 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransactionV3], tx)
    }
  }

  private def assertTxs(first: LeaseTransactionV3, second: LeaseTransactionV3): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.atomicBadge shouldEqual second.atomicBadge
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseTransactionV2") {
    val tx = LeaseTransactionV3
      .create(
        None,
        PublicKeyAccount(senderAccount.publicKey),
        Address.fromString(recipientAccount.address).explicitGet(),
        10000000,
        1000000,
        1526646497465L,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        Proofs(Seq(ByteStr.decodeBase58("5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                        "type": 8,
                        "id": "${tx.id()}",
                        "sender": "${senderAccount.address}",
                        "senderPublicKey": "$senderPkBase58",
                        "fee": 1000000,
                        "timestamp": 1526646497465,
                        "atomicBadge": { "trustedSender": "${senderAccount.address}"},
                        "proofs": [
                        "5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q"
                        ],
                        "version": 3,
                        "amount": 10000000,
                        "recipient": "${recipientAccount.address}"
                       }
    """)

    js shouldEqual tx.json()
  }

  property("forbid assetId in LeaseTransactionV3") {
    val assetIdBytesGen = bytes32gen
    forAll(leaseV3Gen, assetIdBytesGen) { (tx, assetId) =>
      val bytes = tx.bytes()
      // hack in an assetId
      bytes(3) = 1: Byte
      val bytesWithAssetId = bytes.take(4) ++ assetId ++ bytes.drop(4)
      val parsed           = tx.builder.parseBytes(bytesWithAssetId)
      parsed.isFailure shouldBe true
      parsed.failed.get.getMessage.contains("Leasing assets is not supported yet") shouldBe true
    }
  }
}
