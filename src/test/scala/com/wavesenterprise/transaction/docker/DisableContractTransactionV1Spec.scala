package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.transaction.{Proofs, TransactionParsers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class DisableContractTransactionV1Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("DisableContractTransactionV1Spec serialization roundtrip") {
    forAll(disableContractV1ParamGen) { tx: DisableContractTransactionV1 =>
      val recovered = DisableContractTransactionV1.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("DisableContractTransactionV1 proto serialization roundtrip") {
    forAll(disableContractV1ParamGen) { tx =>
      val recovered = DisableContractTransactionV1.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("DisableContractTransactionV1Spec serialization from TypedTransaction") {
    forAll(disableContractV1ParamGen) { tx: DisableContractTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation") {
    val contractId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val timestamp  = System.currentTimeMillis()
    val tx = DisableContractTransactionV1
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        fee = 0,
        timestamp,
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
                       "version": 1,
                       "contractId": "$contractId"
                       }
  """)

    js shouldEqual tx.json()
  }
}
