package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen

import java.nio.charset.StandardCharsets.UTF_8
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.assets.IssueTransactionV2
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class IssueTransactionV2Specification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  property("SmartIssueTransaction serialization roundtrip") {
    forAll(smartIssueTransactionGen()) { tx: IssueTransactionV2 =>
      val recovered = IssueTransactionV2.parseBytes(tx.bytes()).get

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
    forAll(smartIssueTransactionGen()) { tx =>
      val recovered = IssueTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("JSON format validation") {
    val tx = IssueTransactionV2
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
                       "version": 2,
                       "assetId": "${tx.id()}",
                       "chainId": 84,
                       "name": "Gigacoin",
                       "quantity": 10000000000,
                       "reissuable": true,
                       "decimals": 8,
                       "description": "Gigacoin",
                       "script": null
                       }
    """)

    tx.json() shouldEqual js
  }

}
