package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.{AtomicBadge, Proofs, TransactionParsers}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class UpdateContractTransactionV3Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("UpdateContractTransactionV3Spec serialization roundtrip") {
    forAll(updateContractV3ParamGen()) { tx =>
      val recovered = UpdateContractTransactionV3.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("UpdateContractTransactionV3 proto serialization roundtrip") {
    forAll(updateContractV3ParamGen()) { tx =>
      val recovered = UpdateContractTransactionV3.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("UpdateContractTransactionV3Spec serialization from TypedTransaction") {
    forAll(updateContractV3ParamGen()) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation") {
    val timestamp  = System.currentTimeMillis()
    val contractId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val imageHash  = DigestUtils.sha256Hex("some_data")
    val feeAssetId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val tx = UpdateContractTransactionV3
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        "localhost:5000/smart-kv",
        imageHash,
        fee = 0,
        timestamp,
        Some(ByteStr.decodeBase58(feeAssetId).get),
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 107,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 3,
                       "atomicBadge": { "trustedSender": "${senderAccount.address}"},
                       "contractId": "$contractId",
                       "image": "localhost:5000/smart-kv",
                       "imageHash": "$imageHash",
                       "feeAssetId": "$feeAssetId"
                       }
  """)

    js shouldEqual tx.json()
  }
}
