package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.docker.assets.ContractTransferInV1
import com.wavesenterprise.transaction.{AtomicBadge, Proofs, TransactionParsers}
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class CreateContractTransactionV5Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("CreateContractTransactionV5Spec serialization roundtrip") {
    forAll(createContractV5ParamGen()) { tx =>
      val recovered = CreateContractTransactionV5.parseBytes(tx.bytes()).get
      recovered shouldBe tx
      recovered.proofs.bytes() should contain theSameElementsAs tx.proofs.bytes()
    }
  }

  property("CreateContractTransactionV5 proto serialization roundtrip") {
    forAll(createContractV5ParamGen()) { tx =>
      val recovered = CreateContractTransactionV5.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV5Spec serialization from TypedTransaction") {
    forAll(createContractV5ParamGen()) { tx =>
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
    val timestamp          = System.currentTimeMillis()
    val imageHash          = DigestUtils.sha256Hex("some_data")
    val contractName       = "contract"
    val assetId            = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val validationPolicy   = ValidationPolicy.Majority
    val contractApiVersion = ContractApiVersion.`1.0`
    val payments = List(
      ContractTransferInV1(None, 100),
      ContractTransferInV1(Some(ByteStr.decodeBase58(assetId).get), 100)
    )

    val tx = CreateContractTransactionV5
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        "localhost:5000/smart-kv",
        imageHash,
        contractName,
        params,
        fee = 0,
        timestamp,
        feeAssetId = None,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        validationPolicy,
        contractApiVersion,
        payments,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 103,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 5,
                       "atomicBadge": { "trustedSender": "${senderAccount.address}"},
                       "image": "localhost:5000/smart-kv",
                       "imageHash": "$imageHash",
                       "contractName": "$contractName",
                       "params": [
                       {
                       "key": "int",
                       "type": "integer",
                       "value": 24
                       },
                       {
                       "key": "bool",
                       "type": "boolean",
                       "value": true
                       },
                       {
                       "key": "blob",
                       "type": "binary",
                       "value": "base64:YWxpY2U="
                       }
                       ],
                       "feeAssetId": null,
                       "validationPolicy":
                       {
                       "type": "${validationPolicy.name}"
                       },
                       "apiVersion": "${contractApiVersion.toString}",
                       "payments":
                       [
                       {
                       "amount": 100
                       },
                       {
                       "assetId": "$assetId",
                       "amount": 100
                       }
                       ]
                       }
  """)

    js shouldEqual tx.json()
  }
}
