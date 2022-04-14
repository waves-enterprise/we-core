package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.{Proofs, TransactionParsers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import com.wavesenterprise.utils.EitherUtils.EitherExt

class DisableContractTransactionV2Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("DisableContractTransactionV2Spec serialization roundtrip") {
    forAll(disableContractV2ParamGen) { tx: DisableContractTransactionV2 =>
      val recovered = DisableContractTransactionV2.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("DisableContractTransactionV2 proto serialization roundtrip") {
    forAll(disableContractV2ParamGen) { tx =>
      val recovered = DisableContractTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("DisableContractTransactionV2Spec serialization from TypedTransaction") {
    forAll(disableContractV2ParamGen) { tx: DisableContractTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation") {
    val contractId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val timestamp  = System.currentTimeMillis()
    val feeAssetId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val tx = DisableContractTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        fee = 0,
        timestamp,
        Some(ByteStr.decodeBase58(feeAssetId).get),
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 106,
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
                       "feeAssetId": "$feeAssetId"
                       }
  """)

    js shouldEqual tx.json()
  }
}
