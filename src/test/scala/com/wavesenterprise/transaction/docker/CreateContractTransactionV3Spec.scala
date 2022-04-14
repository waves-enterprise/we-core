package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.{AtomicBadge, Proofs, TransactionParsers, ValidationError}
import com.wavesenterprise.utils.Base64
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import com.wavesenterprise.utils.EitherUtils.EitherExt

class CreateContractTransactionV3Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("CreateContractTransactionV3Spec serialization roundtrip") {
    forAll(createContractV3ParamGen()) { tx =>
      val recovered = CreateContractTransactionV3.parseBytes(tx.bytes()).get
      recovered shouldBe tx
      recovered.proofs.bytes() should contain theSameElementsAs tx.proofs.bytes()
    }
  }

  property("CreateContractTransactionV3 proto serialization roundtrip") {
    forAll(createContractV3ParamGen()) { tx =>
      val recovered = CreateContractTransactionV3.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV3Spec serialization from TypedTransaction") {
    forAll(createContractV3ParamGen()) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV3Spec negative validation cases") {
    forAll(createContractV3ParamGen()) {
      case CreateContractTransactionV3(sender, image, imageHash, contractName, params, fee, timestamp, feeAssetId, atomicBadge, proofs) =>
        val emptyKeyParams = List(IntegerDataEntry("", 2))
        val emptyKeyEither =
          CreateContractTransactionV3.create(sender, image, imageHash, contractName, emptyKeyParams, fee, timestamp, feeAssetId, atomicBadge, proofs)
        emptyKeyEither shouldBe Left(ValidationError.GenericError("Param with empty key was found"))

        val duplicateKeysParams = List(IntegerDataEntry("key1", 2), StringDataEntry("key1", "value"))
        val duplicateKeysEither =
          CreateContractTransactionV3.create(sender,
                                             image,
                                             imageHash,
                                             contractName,
                                             duplicateKeysParams,
                                             fee,
                                             timestamp,
                                             feeAssetId,
                                             atomicBadge,
                                             proofs)
        duplicateKeysEither shouldBe Left(ValidationError.GenericError("Params with duplicate keys were found"))

        val tooBigTxParams = List(BinaryDataEntry("key1", ByteStr(Array.fill(ExecutableTransaction.MaxBytes)(1: Byte))))
        val tooBigTxEither =
          CreateContractTransactionV3.create(sender, image, imageHash, contractName, tooBigTxParams, fee, timestamp, feeAssetId, atomicBadge, proofs)
        tooBigTxEither.left.get shouldBe a[ValidationError.ContractTransactionTooBig]

        val invalidImageHash = "some_string"
        val invalidImageHashTx =
          CreateContractTransactionV3.create(sender,
                                             image,
                                             invalidImageHash,
                                             contractName,
                                             tooBigTxParams,
                                             fee,
                                             timestamp,
                                             feeAssetId,
                                             atomicBadge,
                                             proofs)
        invalidImageHashTx shouldBe Left(ValidationError.GenericError(s"Image hash string $invalidImageHash is not valid SHA-256 hex string"))
        val withNonAsciiKeysParams = List(IntegerDataEntry("key∂√1", 2), StringDataEntry("kååey1", "value"))
        val withNonAsciiKeysEither =
          CreateContractTransactionV3.create(sender,
                                             image,
                                             imageHash,
                                             contractName,
                                             withNonAsciiKeysParams,
                                             fee,
                                             timestamp,
                                             feeAssetId,
                                             atomicBadge,
                                             proofs)
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
    val tx = CreateContractTransactionV3
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
                       "version": 3,
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
                       "feeAssetId": null
                       }
  """)

    js shouldEqual tx.json()
  }
}
