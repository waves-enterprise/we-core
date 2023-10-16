package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.docker.assets.ContractTransferInV1
import com.wavesenterprise.transaction.{AtomicBadge, Proofs, TransactionParsers}
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import tools.GenHelper._

class CallContractTransactionV6Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("CallContractTransactionV6Spec serialization roundtrip") {
    forAll(callContractV6ParamGen()) { tx =>
      val recovered = CallContractTransactionV6.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("CallContractTransactionV6 proto serialization roundtrip") {
    forAll(callContractV6ParamGen()) { tx =>
      val recovered = CallContractTransactionV6.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("CallContractTransactionV6Spec serialization from TypedTransaction") {
    forAll(callContractV6ParamGen()) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation") {
    val params = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp       = System.currentTimeMillis()
    val contractId      = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val contractVersion = 1
    val feeAssetId      = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val payments = List(
      ContractTransferInV1(None, 100),
      ContractTransferInV1(Some(ByteStr.decodeBase58(feeAssetId).get), 100)
    )

    val inputCommitment = commitmentGen().generateSample()

    val tx = CallContractTransactionV6
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        params,
        fee = 0,
        timestamp,
        contractVersion,
        Some(ByteStr.decodeBase58(feeAssetId).get),
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        payments,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get)),
        inputCommitment
      )
      .right
      .get

    val js = Json.parse(
      s"""
         |{
         |   "type":104,
         |   "id":"${tx.id()}",
         |   "sender":"${senderAccount.address}",
         |   "senderPublicKey":"$senderPkBase58",
         |   "fee":0,
         |   "timestamp": $timestamp,
         |   "proofs":[
         |      "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
         |   ],
         |   "version":6,
         |   "atomicBadge":{
         |      "trustedSender":"${senderAccount.address}"
         |   },
         |   "contractId":"$contractId",
         |   "contractVersion": $contractVersion,
         |   "feeAssetId":"$feeAssetId",
         |   "params":[
         |      {
         |         "key":"int",
         |         "type":"integer",
         |         "value":24
         |      },
         |      {
         |         "key":"bool",
         |         "type":"boolean",
         |         "value":true
         |      },
         |      {
         |         "key":"blob",
         |         "type":"binary",
         |         "value":"base64:YWxpY2U="
         |      }
         |   ],
         |   "payments":[
         |      {
         |         "amount":100
         |      },
         |      {
         |         "assetId":"$feeAssetId",
         |         "amount":100
         |      }
         |   ],
         |   "inputCommitment":"${inputCommitment.hash.toString}"
         |}
         |""".stripMargin
    )

    js shouldEqual tx.json()
  }
}
