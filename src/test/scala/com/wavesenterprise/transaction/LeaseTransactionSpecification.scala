package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.{Address, PublicKeyAccount}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.transaction.lease.{LeaseTransaction, LeaseTransactionV2}
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class LeaseTransactionSpecification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen with WithSenderAndRecipient {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get.asInstanceOf[LeaseTransaction]
      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction proto serialization roundtrip") {
    forAll(leaseGen) { tx =>
      val recovered = LeaseTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      tx shouldEqual recovered
    }
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseTransactionV2") {
    val tx = LeaseTransactionV2
      .create(
        None,
        PublicKeyAccount(senderAccount.publicKey),
        Address.fromString(recipientAccount.address).explicitGet(),
        10000000,
        1000000,
        1526646497465L,
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
                        "proofs": [
                        "5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q"
                        ],
                        "version": 2,
                        "amount": 10000000,
                        "recipient": "${recipientAccount.address}"
                       }
    """)

    js shouldEqual tx.json()
  }

  property("forbid assetId in LeaseTransactionV2") {
    val leaseV2Gen      = leaseGen.filter(_.version == 2)
    val assetIdBytesGen = bytes32gen
    forAll(leaseV2Gen, assetIdBytesGen) { (tx, assetId) =>
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
