package com.wavesenterprise.transaction

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.state.diffs.ProduceError.produce
import com.wavesenterprise.transaction.assets.{IssueTransactionV2, SponsorFeeTransactionV2}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.utils.NumberUtils.DoubleExt
import com.wavesenterprise.utils.{Base58, Base64}
import com.wavesenterprise.{CoreTransactionGen, crypto}
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class SponsorFeeTransactionV2Specification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  val One = 100000000L

  property("SponsorFee serialization roundtrip") {
    forAll(sponsorFeeV2Gen) { transaction: SponsorFeeTransactionV2 =>
      val recovered = SponsorFeeTransactionV2.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee proto serialization roundtrip") {
    forAll(sponsorFeeV2Gen) { tx =>
      val recovered = SponsorFeeTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("decode pre-encoded bytes") {
    val bytes = Base64
      .decode(
        "AA4Cq51Ian2uSwACGAGBzNt4uEHJtyW9ETP9JMt8bL0h3V78HoilaiX8mTit/TJLzSEiwf8pVDU9MnKD0ixgj0eqGgEAAAAABfXhAAAADF1swIRMAQEBVPznzmNoZG0ceSb5oaSFLEBIq/5D00kJnQEAAQBAqn0Ni1Hr9+tLGO3qEVyoMTVI/lFSE9WnGE1v3KYwfLDzg+3wv2p9z/1tA2FzD8RJ0+g3+KvzUbiEZ2TtiHajjQ==")
      .get

    val json = Json.parse(s"""
                            |{
                            |    "senderPublicKey":"CYupT3zpgDaxTbXmnNa7psKR7bKNdJycTssjwuDzspkd",
                            |    "atomicBadge": { "trustedSender": "3NCyKkSRsirFSSHGzq16RdHhDchEMVnTeVr"},
                            |    "sender":"3NCyKkSRsirFSSHGzq16RdHhDchEMVnTeVr",
                            |    "proofs":[
                            |        "4QhZu5VmKCeiYrcuk8La28yN3QPbAtPrWMV53dNQLTHNC6QHWFkuDzPaS28VGpMhk4fVQo1sUmvdFez8NQzcrUJp"
                            |    ],
                            |    "assetId":"HyAkA27DbuhLcFmimoSXoD9H5FP99JX5PcSXvMni4UWM",
                            |    "fee":100000000,
                            |    "isEnabled": true,
                            |    "id":"GoFvyEVxzEfHJWJcf9rsQQFBmDXw66sayP9uKXWxuUon",
                            |    "type":14,
                            |    "version":2,
                            |    "timestamp":13595396047948
                            |}
                            |""".stripMargin)

    val txFromBytes = SponsorFeeTransactionV2.parseBytes(bytes).get
    txFromBytes.json() shouldBe json
    assert(
      crypto.verify(txFromBytes.proofs.proofs.head.arr, txFromBytes.bodyBytes(), txFromBytes.sender.publicKey.getEncoded),
      "signature should be valid"
    )
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(sponsorFeeV2Gen) { transaction: SponsorFeeTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("JSON format validation") {
    val tx = SponsorFeeTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        isEnabled = true,
        fee = One,
        timestamp = 1520945679531L,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
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
                         "atomicBadge": { "trustedSender": "${senderAccount.address}"},
                         "proofs": [
                         "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
                         ],
                         "version": 2,
                         "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                         "isEnabled": true
                       }""")

    js shouldEqual tx.json()
  }

  property("JSON format validation for canceling sponsorship") {
    val tx = SponsorFeeTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        isEnabled = false,
        One,
        1520945679531L,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .right
      .get

    val tx1 = SponsorFeeTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        isEnabled = false,
        One,
        1520945679531L,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
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
         |"atomicBadge": { "trustedSender": "${senderAccount.address}"},
         |"proofs": ["3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"],
         |"version": 2,
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
         |"atomicBadge": { "trustedSender": "${senderAccount.address}"},
         |"proofs": ["3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"],
         |"version": 2,
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
      } yield SponsorFeeTransactionV2.selfSigned(sender,
                                                 assetId,
                                                 isEnabled = true,
                                                 fee,
                                                 timestamp,
                                                 Some(AtomicBadge(Some(senderAccount.toAddress)))) should produce("insufficient fee")
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
      } yield SponsorFeeTransactionV2.selfSigned(sender,
                                                 assetId,
                                                 isEnabled = false,
                                                 fee,
                                                 timestamp,
                                                 Some(AtomicBadge(Some(senderAccount.toAddress)))) should produce("insufficient fee")
    }
  }
}
