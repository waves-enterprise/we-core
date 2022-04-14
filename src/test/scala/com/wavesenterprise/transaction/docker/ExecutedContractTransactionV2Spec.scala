package com.wavesenterprise.transaction.docker

import com.wavesenterprise.NoShrink
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, IntegerDataEntry}
import com.wavesenterprise.transaction.{Proofs, TransactionParsers}
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import tools.GenHelper._

class ExecutedContractTransactionV2Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen with NoShrink {

  property("ExecutedContractTransactionV2 serialization roundtrip") {
    forAll(executedContractV2ParamGen) { tx: ExecutedContractTransactionV2 =>
      val recovered = ExecutedContractTransactionV2.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
    forAll(bigExecutedContractV2ParamGen) { tx: ExecutedContractTransactionV2 =>
      val recovered = ExecutedContractTransactionV2.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("ExecutedContractTransactionV2 proto serialization roundtrip") {
    forAll(executedContractV2ParamGen) { tx =>
      val recovered = ExecutedContractTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
    forAll(bigExecutedContractV2ParamGen) { tx =>
      val recovered = ExecutedContractTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("ExecutedContractTransactionV2 serialization from TypedTransaction") {
    forAll(executedContractV2ParamGen) { tx: ExecutedContractTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
    forAll(bigExecutedContractV2ParamGen) { tx: ExecutedContractTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation with CreateContractTransactionV1") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp    = System.currentTimeMillis()
    val image        = "localhost:5000/smart-kv"
    val imageHash    = DigestUtils.sha256Hex("some_data")
    val contractName = "contract"
    val txTimestamp  = timestamp - 1000
    val tx = CreateContractTransactionV1
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        image = image,
        imageHash = imageHash,
        contractName = contractName,
        params = testDataEntries,
        fee = 0,
        timestamp = txTimestamp,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV2
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 105,
                       "id": "${execTx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "resultsHash": "$resultsHash",
                       "validationProofs": ${Json.toJson(validationProofs)},
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 2,
                       "tx": {
                           "type": 103,
                           "id": "${tx.id()}",
                           "sender": "${senderAccount.address}",
                           "senderPublicKey": "$senderPkBase58",
                           "fee": 0,
                           "timestamp": $txTimestamp,
                           "proofs": [
                           "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                           ],
                           "version": 1,
                           "image": "$image",
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
                           ]
                       },
                       "results": [
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
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }

  property("JSON format validation with CreateContractTransactionV2") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp    = System.currentTimeMillis()
    val image        = "localhost:5000/smart-kv"
    val imageHash    = DigestUtils.sha256Hex("some_data")
    val contractName = "contract"
    val txTimestamp  = timestamp - 1000
    val tx = CreateContractTransactionV2
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        image = image,
        imageHash = imageHash,
        contractName = contractName,
        params = testDataEntries,
        fee = 0,
        feeAssetId = None,
        timestamp = txTimestamp,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV2
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 105,
                       "id": "${execTx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "resultsHash": "$resultsHash",
                       "validationProofs": ${Json.toJson(validationProofs)},
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 2,
                       "tx": {
                           "type": 103,
                           "id": "${tx.id()}",
                           "sender": "${senderAccount.address}",
                           "senderPublicKey": "$senderPkBase58",
                           "fee": 0,
                           "timestamp": $txTimestamp,
                           "proofs": [
                           "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                           ],
                           "version": 2,
                           "image": "$image",
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
                       },
                       "results": [
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
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }

  property("JSON format validation with CallContractTransactionV1") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp   = System.currentTimeMillis()
    val contractId  = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val txTimestamp = timestamp - 1000
    val tx = CallContractTransactionV1
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        contractId = ByteStr.decodeBase58(contractId).get,
        params = testDataEntries,
        fee = 0,
        timestamp = txTimestamp,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        tx,
        results = testDataEntries,
        resultsHash,
        validationProofs,
        timestamp,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 105,
                       "id": "${execTx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "resultsHash": "$resultsHash",
                       "validationProofs": ${Json.toJson(validationProofs)},
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 2,
                       "tx": {
                           "type": 104,
                           "id": "${tx.id()}",
                           "sender": "${senderAccount.address}",
                           "senderPublicKey": "$senderPkBase58",
                           "fee": 0,
                           "timestamp": $txTimestamp,
                           "proofs": [
                           "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                           ],
                           "version": 1,
                           "contractId": "$contractId",
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
                           ]
                       },
                       "results": [
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
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }

  property("JSON format validation with CallContractTransactionV2") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp       = System.currentTimeMillis()
    val contractId      = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val txTimestamp     = timestamp - 1000
    val contractVersion = 1
    val tx = CallContractTransactionV2
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        contractId = ByteStr.decodeBase58(contractId).get,
        params = testDataEntries,
        fee = 0,
        timestamp = txTimestamp,
        contractVersion = contractVersion,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV2
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 105,
                       "id": "${execTx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "resultsHash": "$resultsHash",
                       "validationProofs": ${Json.toJson(validationProofs)},
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 2,
                       "tx": {
                           "type": 104,
                           "id": "${tx.id()}",
                           "sender": "${senderAccount.address}",
                           "senderPublicKey": "$senderPkBase58",
                           "fee": 0,
                           "timestamp": $txTimestamp,
                           "proofs": [
                           "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                           ],
                           "version": 2,
                           "contractId": "$contractId",
                           "contractVersion": $contractVersion,
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
                           ]
                       },
                       "results": [
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
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }

  property("JSON format validation with CallContractTransactionV3") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp       = System.currentTimeMillis()
    val contractId      = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val txTimestamp     = timestamp - 1000
    val contractVersion = 1
    val feeAssetId      = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val tx = CallContractTransactionV3
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        testDataEntries,
        fee = 0,
        txTimestamp,
        contractVersion,
        Some(ByteStr.decodeBase58(feeAssetId).get),
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV2
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 105,
                       "id": "${execTx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "resultsHash": "$resultsHash",
                       "validationProofs": ${Json.toJson(validationProofs)},
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 2,
                       "tx": {
                           "type": 104,
                           "id": "${tx.id()}",
                           "sender": "${senderAccount.address}",
                           "senderPublicKey": "$senderPkBase58",
                           "fee": 0,
                           "timestamp": $txTimestamp,
                           "proofs": [
                           "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                           ],
                           "version": 3,
                           "contractId": "$contractId",
                           "contractVersion": $contractVersion,
                           "feeAssetId": "$feeAssetId",
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
                           ]
                       },
                       "results": [
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
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }
}
