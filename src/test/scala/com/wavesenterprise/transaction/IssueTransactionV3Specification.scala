package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.assets.IssueTransactionV3
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets.UTF_8

class IssueTransactionV3Specification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  property("SmartIssueTransaction serialization roundtrip") {
    forAll(smartIssueTransactionV3Gen()) { tx: IssueTransactionV3 =>
      val recovered = IssueTransactionV3.parseBytes(tx.bytes()).get

      tx.sender.address shouldEqual recovered.sender.address
      tx.timestamp shouldEqual recovered.timestamp
      tx.decimals shouldEqual recovered.decimals
      tx.description shouldEqual recovered.description
      tx.script shouldEqual recovered.script
      tx.reissuable shouldEqual recovered.reissuable
      tx.fee shouldEqual recovered.fee
      tx.name shouldEqual recovered.name
      tx.chainId shouldEqual recovered.chainId
      tx.bytes() shouldEqual recovered.bytes()
    }
  }

  property("SmartIssueTransaction proto serialization roundtrip") {
    forAll(smartIssueTransactionV3Gen()) { tx =>
      val recovered = IssueTransactionV3.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("JSON format validation") {
    val tx = IssueTransactionV3
      .create(
        'T',
        PublicKeyAccount(senderAccount.publicKey),
        "Gigacoin".getBytes(UTF_8),
        "Gigacoin".getBytes(UTF_8),
        10000000000L,
        8,
        true,
        100000000,
        1526287561757L,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        None,
        Proofs(Seq(ByteStr.decodeBase58("43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 3,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 100000000,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g"
                       ],
                       "version": 3,
                       "assetId": "${tx.id()}",
                       "chainId": 84,
                       "name": "Gigacoin",
                       "quantity": 10000000000,
                       "reissuable": true,
                       "decimals": 8,
                       "description": "Gigacoin",
                       "atomicBadge": { "trustedSender": "${senderAccount.address}"},
                       "script": null
                       }
    """)

    tx.json() shouldEqual js
  }

}
