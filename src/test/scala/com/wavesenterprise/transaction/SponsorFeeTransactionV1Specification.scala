package com.wavesenterprise.transaction

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.state.diffs.ProduceError.produce
import com.wavesenterprise.transaction.assets.{IssueTransactionV2, SponsorFeeTransactionV1}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.utils.NumberUtils.DoubleExt
import com.wavesenterprise.utils.{Base58, Base64}
import com.wavesenterprise.{CoreTransactionGen, crypto}
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class SponsorFeeTransactionV1Specification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  val One = 100000000L

  property("SponsorFee serialization roundtrip") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransactionV1 =>
      val recovered = SponsorFeeTransactionV1.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee proto serialization roundtrip") {
    forAll(sponsorFeeGen) { tx =>
      val recovered = SponsorFeeTransactionV1.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("decode pre-encoded bytes") {
    val bytes = Base64
      .decode(
        "AA4BiwVmyV2enGimp8W/YjJhRDGdvAg9Hlj+GvD13CALUR38HoilaiX8mTit/TJLzSEiwf8pVDU9MnKD0ixgj0eqGgEAAAAABfXhAAAADF1swIRMAQABAEClZcAaaWRKVc5wU3HXO14a72L581OqDriZFbEzU3H8tT5p7oapzVjYW5w56UxAUsrxOcul6l2857M1lIu7D20D"
      )
      .get

    val json = Json.parse("""
                            |{
                            |
                            |    "senderPublicKey":"AMgSw5s41d9smGTBHKQT6cJ4geXnqfwCz3petMWnhLDA",
                            |    "sender":"3MwWjDENjzxMeUrJ2hv34YCEGtX3ZaNr8CM",
                            |    "proofs":[
                            |        "4JoAFJNt1j1AGDW5he5jAfvFgcxUu3XE17EZkC9qdsAVhkQ7vaH3YRP21XJ1rFAUVVZvSiBDeo2Eojsqx9Fbp54E"
                            |    ],
                            |    "assetId":"HyAkA27DbuhLcFmimoSXoD9H5FP99JX5PcSXvMni4UWM",
                            |    "fee":100000000,
                            |    "isEnabled": true,
                            |    "id":"6YDMsYp2YtdbWekRUKxRnYc6HWSnRr7fnojQoAktQ7vk",
                            |    "type":14,
                            |    "version":1,
                            |    "timestamp":13595396047948
                            |
                            |}
                            |""".stripMargin)

    val txFromBytes = SponsorFeeTransactionV1.parseBytes(bytes).get
    txFromBytes.json() shouldBe json
    assert(
      crypto.verify(txFromBytes.proofs.proofs.head.arr, txFromBytes.bodyBytes(), txFromBytes.sender.publicKey.getEncoded),
      "signature should be valid"
    )
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("JSON format validation") {
    val tx = SponsorFeeTransactionV1
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        isEnabled = true,
        fee = One,
        timestamp = 1520945679531L,
        proofs = Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                         "type": 14,
                         "id": "${tx.id()}",
                         "sender": "${senderAccount.address}",
                         "senderPublicKey": "$senderPkBase58",
                         "fee": 100000000,
                         "timestamp": 1520945679531,
                         "proofs": [
                         "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
                         ],
                         "version": 1,
                         "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                         "isEnabled": true
                       }""")

    js shouldEqual tx.json()
  }

  property("JSON format validation for canceling sponsorship") {
    val tx = SponsorFeeTransactionV1
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        isEnabled = false,
        One,
        1520945679531L,
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .right
      .get

    val tx1 = SponsorFeeTransactionV1
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        isEnabled = false,
        One,
        1520945679531L,
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
         |"type": 14,
         |"id": "${tx.id()}",
         |"sender": "${senderAccount.address}",
         |"senderPublicKey": "${Base58.encode(senderAccount.publicKey.getEncoded)}",
         |"fee": $One,
         |"timestamp": 1520945679531,
         |"proofs": ["3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"],
         |"version": 1,
         |"assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
         |"isEnabled": false
       }""".stripMargin)

    val js1 = Json.parse(s"""{
         |"type": 14,
         |"id": "${tx1.id()}",
         |"sender": "${senderAccount.address}",
         |"senderPublicKey": "${Base58.encode(senderAccount.publicKey.getEncoded)}",
         |"fee": $One,
         |"timestamp": 1520945679531,
         |"proofs": ["3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"],
         |"version": 1,
         |"assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
         |"isEnabled": false
       }""".stripMargin)

    js shouldEqual tx.json()
    js1 shouldEqual tx1.json()
  }
  val invalidFee =
    Table(
      "fee",
      -1.west,
      0
    )

  property("sponsorship negative fee") {
    forAll(invalidFee) { fee: Long =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransactionV2
          .selfSigned(currentChainId, sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp, None)
          .right
          .get
        assetId = issue.assetId()
      } yield SponsorFeeTransactionV1.selfSigned(sender, assetId, isEnabled = true, fee, timestamp) should produce("insufficient fee")
    }
  }

  property("cancel sponsorship negative fee") {
    forAll(invalidFee) { fee: Long =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransactionV2
          .selfSigned(currentChainId, sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp, None)
          .right
          .get
        assetId = issue.assetId()
      } yield SponsorFeeTransactionV1.selfSigned(sender, assetId, isEnabled = false, fee, timestamp) should produce("insufficient fee")
    }
  }
}
