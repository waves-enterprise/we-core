package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.{Proofs, TransactionParsers}
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import com.wavesenterprise.utils.EitherUtils.EitherExt

class UpdateContractTransactionV2Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("UpdateContractTransactionV2Spec serialization roundtrip") {
    forAll(updateContractV2ParamGen) { tx: UpdateContractTransactionV2 =>
      val recovered = UpdateContractTransactionV2.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("UpdateContractTransactionV2 proto serialization roundtrip") {
    forAll(updateContractV2ParamGen) { tx =>
      val recovered = UpdateContractTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("UpdateContractTransactionV2Spec serialization from TypedTransaction") {
    forAll(updateContractV2ParamGen) { tx: UpdateContractTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation") {
    val timestamp  = System.currentTimeMillis()
    val contractId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val imageHash  = DigestUtils.sha256Hex("some_data")
    val feeAssetId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val tx = UpdateContractTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        "localhost:5000/smart-kv",
        imageHash,
        fee = 0,
        timestamp,
        Some(ByteStr.decodeBase58(feeAssetId).get),
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
                       "version": 2,
                       "contractId": "$contractId",
                       "image": "localhost:5000/smart-kv",
                       "imageHash": "$imageHash",
                       "feeAssetId": "$feeAssetId"
                       }
  """)

    js shouldEqual tx.json()
  }
}
