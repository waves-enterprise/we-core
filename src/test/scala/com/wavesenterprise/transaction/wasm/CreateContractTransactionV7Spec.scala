package com.wavesenterprise.transaction.wasm

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.StoredContract.WasmContract
import com.wavesenterprise.javadsl.docker.validator.ValidationPolicy
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.docker.{ContractTransactionGen, CreateContractTransactionV7, ExecutableTransaction}
import com.wavesenterprise.transaction.{Proofs, TransactionParsers, ValidationError}
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets.UTF_8

class CreateContractTransactionV7Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("CreateContractTransactionV7Spec serialization roundtrip") {
    forAll(createContractV7ParamGen) { tx: CreateContractTransactionV7 =>
      val recovered = CreateContractTransactionV7.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV7 proto serialization roundtrip") {
    forAll(createContractV7ParamGen) { tx =>
      val recovered = CreateContractTransactionV7.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV7Spec serialization from TypedTransaction") {
    forAll(createContractV7ParamGen) { tx: CreateContractTransactionV7 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("CreateContractTransactionV7Spec negative validation cases") {
    forAll(createContractV7ParamGen) {
      case CreateContractTransactionV7(
            sender,
            contractName,
            params,
            fee,
            timestamp,
            feeAssetId,
            atomicBadge,
            validationPolicy,
            payments,
            isConfidential,
            groupParticipants,
            groupOwners,
            storedContract,
            proofs
          ) =>
        val emptyKeyParams = List(IntegerDataEntry("", 2))
        val emptyKeyEither =
          CreateContractTransactionV7.create(
            sender,
            contractName,
            emptyKeyParams,
            fee,
            timestamp,
            feeAssetId,
            atomicBadge,
            validationPolicy,
            payments,
            isConfidential,
            groupParticipants,
            groupOwners,
            storedContract,
            proofs
          )
        emptyKeyEither shouldBe Left(ValidationError.GenericError("Param with empty key was found"))

        val duplicateKeysParams = List(IntegerDataEntry("key1", 2), StringDataEntry("key1", "value"))
        val duplicateKeysEither =
          CreateContractTransactionV7.create(
            sender,
            contractName,
            duplicateKeysParams,
            fee,
            timestamp,
            feeAssetId,
            atomicBadge,
            validationPolicy,
            payments,
            isConfidential,
            groupParticipants,
            groupOwners,
            storedContract,
            proofs
          )
        duplicateKeysEither shouldBe Left(ValidationError.GenericError("Params with duplicate keys were found"))

        val tooBigTxParams = List(BinaryDataEntry("key1", ByteStr(Array.fill(ExecutableTransaction.MaxBytes)(1: Byte))))
        val tooBigTxEither =
          CreateContractTransactionV7.create(
            sender,
            contractName,
            tooBigTxParams,
            fee,
            timestamp,
            feeAssetId,
            atomicBadge,
            validationPolicy,
            payments,
            isConfidential,
            groupParticipants,
            groupOwners,
            storedContract,
            proofs
          )
        tooBigTxEither.left.get shouldBe a[ValidationError.ContractTransactionTooBig]

        val withNonAsciiKeysParams = List(IntegerDataEntry("key∂√1", 2), StringDataEntry("kååey1", "value"))
        val withNonAsciiKeysEither =
          CreateContractTransactionV7.create(
            sender,
            contractName,
            withNonAsciiKeysParams,
            fee,
            timestamp,
            feeAssetId,
            atomicBadge,
            validationPolicy,
            payments,
            isConfidential,
            groupParticipants,
            groupOwners,
            storedContract,
            proofs
          )
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
    val bytecode     = "some_data".getBytes(UTF_8)
    val bytecodeHash = DigestUtils.sha256Hex("some_data")
    val contractName = "contract"
    val tx = CreateContractTransactionV7
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        contractName = contractName,
        params = params,
        fee = 0,
        timestamp = timestamp,
        feeAssetId = None,
        atomicBadge = None,
        validationPolicy = ValidationPolicy.ANY,
        payments = List.empty,
        isConfidential = false,
        groupParticipants = Set.empty,
        groupOwners = Set.empty,
        storedContract = WasmContract(bytecode, bytecodeHash),
        proofs = Proofs(
          Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get)
        )
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
                       "version": 7,
                       "storedContract": {
                          "bytecodeHash": "$bytecodeHash",
                          "bytecode": "${Base64.encode("some_data".getBytes(UTF_8))}"
                       },
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
                       "atomicBadge": null,
                       "isConfidential": false,
                       "validationPolicy": {"type":"any"},
                       "payments": [],
                       "groupParticipants": [],
                       "groupOwners": [],
                       "feeAssetId": null
                       }
  """)

    js shouldEqual tx.json()
  }
}
