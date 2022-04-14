package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state._
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.transaction.{Proofs, TransactionParsers, ValidationError}
import com.wavesenterprise.utils.Base64
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class CreateContractTransactionV2Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("CreateContractTransactionV2Spec serialization roundtrip") {
    forAll(createContractV2ParamGen) { tx: CreateContractTransactionV2 =>
      val recovered = CreateContractTransactionV2.parseBytes(tx.bytes()).get
      recovered shouldBe tx
      recovered.proofs.bytes() should contain theSameElementsAs tx.proofs.bytes()
    }
  }

  property("CreateContractTransactionV2 proto serialization roundtrip") {
    forAll(createContractV2ParamGen) { tx =>
      val recovered = CreateContractTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV2Spec serialization from TypedTransaction") {
    forAll(createContractV2ParamGen) { tx: CreateContractTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV2Spec negative validation cases") {
    forAll(createContractV2ParamGen) {
      case CreateContractTransactionV2(sender, image, imageHash, contractName, params, fee, feeAssetId, timestamp, proofs) =>
        val emptyKeyParams = List(IntegerDataEntry("", 2))
        val emptyKeyEither =
          CreateContractTransactionV2.create(sender, image, imageHash, contractName, emptyKeyParams, fee, feeAssetId, timestamp, proofs)
        emptyKeyEither shouldBe Left(ValidationError.GenericError("Param with empty key was found"))

        val duplicateKeysParams = List(IntegerDataEntry("key1", 2), StringDataEntry("key1", "value"))
        val duplicateKeysEither =
          CreateContractTransactionV2.create(sender, image, imageHash, contractName, duplicateKeysParams, fee, feeAssetId, timestamp, proofs)
        duplicateKeysEither shouldBe Left(ValidationError.GenericError("Params with duplicate keys were found"))

        val tooBigTxParams = List(BinaryDataEntry("key1", ByteStr(Array.fill(ExecutableTransaction.MaxBytes)(1: Byte))))
        val tooBigTxEither =
          CreateContractTransactionV2.create(sender, image, imageHash, contractName, tooBigTxParams, fee, feeAssetId, timestamp, proofs)
        tooBigTxEither.left.get shouldBe a[ValidationError.ContractTransactionTooBig]

        val invalidImageHash = "some_string"
        val invalidImageHashTx =
          CreateContractTransactionV2.create(sender, image, invalidImageHash, contractName, tooBigTxParams, fee, feeAssetId, timestamp, proofs)
        invalidImageHashTx shouldBe Left(ValidationError.GenericError(s"Image hash string $invalidImageHash is not valid SHA-256 hex string"))

        val withNonAsciiKeysParams = List(IntegerDataEntry("key∂√1", 2), StringDataEntry("kååey1", "value"))
        val withNonAsciiKeysEither =
          CreateContractTransactionV2.create(sender, image, imageHash, contractName, withNonAsciiKeysParams, fee, feeAssetId, timestamp, proofs)
        withNonAsciiKeysEither shouldBe Left(ValidationError.InvalidContractKeys("key∂√1 -> ∂√; kååey1 -> å"))
    }
  }

  property("JSON format validation") {
    val params = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp    = System.currentTimeMillis()
    val imageHash    = DigestUtils.sha256Hex("some_data")
    val contractName = "contract"
    val tx = CreateContractTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        "localhost:5000/smart-kv",
        imageHash,
        contractName,
        params,
        fee = 0,
        timestamp,
        feeAssetId = None,
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
                       "version": 2,
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
                       "feeAssetId": null
                       }
  """)

    js shouldEqual tx.json()
  }
}
