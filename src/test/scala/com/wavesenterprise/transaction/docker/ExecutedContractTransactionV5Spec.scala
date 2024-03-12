package com.wavesenterprise.transaction.docker

import cats.implicits.catsSyntaxOptionId
import com.wavesenterprise.NoShrink
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.StoredContract.DockerContract
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.lang.EitherExt
import com.wavesenterprise.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, IntegerDataEntry}
import com.wavesenterprise.transaction.docker.ContractTransactionEntryOps.DataEntryMap
import com.wavesenterprise.transaction.docker.assets.ContractAssetOperation.ContractAssetOperationMap
import com.wavesenterprise.transaction.docker.assets.ContractAssetOperation
import com.wavesenterprise.transaction.{AtomicBadge, Proofs, TransactionParsers, WithSenderAndRecipient}
import com.wavesenterprise.utils.Base64
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import tools.GenHelper.ExtendedGen

class ExecutedContractTransactionV5Spec
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ContractTransactionGen
    with NoShrink
    with WithSenderAndRecipient {

  property("ExecutedContractTransactionV5 serialization roundtrip") {
    forAll(executedContractV5ParamGen()) { tx: ExecutedContractTransactionV5 =>
      val recovered = ExecutedContractTransactionV5.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
    forAll(executedContractV5ParamGen(20)) { tx: ExecutedContractTransactionV5 =>
      val recovered = ExecutedContractTransactionV5.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("ExecutedContractTransactionV5 proto serialization roundtrip") {
    forAll(executedContractV5ParamGen()) { tx =>
      val recovered = ExecutedContractTransactionV5.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
    forAll(executedContractV5ParamGen(20)) { tx =>
      val recovered = ExecutedContractTransactionV5.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("ExecutedContractTransactionV5 serialization from TypedTransaction") {
    forAll(executedContractV5ParamGen()) { tx: ExecutedContractTransactionV5 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
    forAll(executedContractV5ParamGen(20)) { tx: ExecutedContractTransactionV5 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation with CreateContractTransactionV7") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp    = System.currentTimeMillis()
    val image        = "localhost:5000/smart-kv"
    val imageHash    = DigestUtils.sha256Hex("some_data")
    val apiVersion   = ContractApiVersion.Current
    val contractName = "contract"
    val txTimestamp  = timestamp - 1000
    val tx = CreateContractTransactionV7
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        contractName = contractName,
        params = testDataEntries,
        fee = 0,
        timestamp = txTimestamp,
        feeAssetId = None,
        atomicBadge = None,
        validationPolicy = ValidationPolicy.Any,
        payments = List.empty,
        isConfidential = false,
        groupParticipants = Set.empty,
        groupOwners = Set.empty,
        storedContract = DockerContract(
          image = image,
          imageHash = imageHash,
          apiVersion = apiVersion
        ),
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val nonce   = Byte.MaxValue
    val assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get

    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val dataEntryMap       = DataEntryMap(Map(tx.contractId -> testDataEntries))
    val assetOperationsMap = ContractAssetOperationMap(Map(tx.contractId -> assetOperations))

    val resultsHash = ContractTransactionValidation.resultsMapHash(
      dataEntryMap,
      assetOperationsMap
    )
    val validationProofs = validationProofsGen(resultsHash).generateSample()
    val readings         = readingsGen.generateSample()
    val readingsHash     = ReadingsHash(digestSizeByteStrGen.generateSample())
    val outputCommitment = commitmentGen().generateSample()

    val execTx = ExecutedContractTransactionV5
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        resultsMap = dataEntryMap,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get)),
        assetOperationsMap = assetOperationsMap,
        readings = readings,
        readingsHash = readingsHash.some,
        outputCommitment = outputCommitment.some,
        statusCode = 0,
        errorMessage = None
      )
      .right
      .get

    val js = Json.parse(
      s"""
         |{
         |   "senderPublicKey":"$senderPkBase58",
         |   "statusCode" : 0,
         |   "errorMessage": null,
         |   "tx":{
         |      "senderPublicKey":"$senderPkBase58",
         |      "isConfidential": false,
         |      "fee":0,
         |      "feeAssetId": null,
         |      "type":103,
         |      "id":"${tx.id()}",
         |      "sender":"${senderAccount.address}",
         |      "timestamp": $txTimestamp,
         |      "atomicBadge": null,
         |      "validationPolicy": {"type": "any"},
         |      "payments": [],
         |      "groupOwners": [],
         |      "groupParticipants": [],
         |      "proofs":[
         |         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
         |      ],
         |      "version":7,
         |      "storedContract":  {
         |        "image":"$image",
         |        "imageHash":"$imageHash",
         |        "apiVersion": "1.10"
         |      },
         |      "contractName":"$contractName",
         |      "params":[
         |         {
         |            "key":"int",
         |            "type":"integer",
         |            "value":24
         |         },
         |         {
         |            "key":"bool",
         |            "type":"boolean",
         |            "value":true
         |         },
         |         {
         |            "key":"blob",
         |            "type":"binary",
         |            "value":"base64:YWxpY2U="
         |         }
         |      ]
         |   },
         |   "resultsMap": {
         |     "${tx.contractId.base58}": [
         |        {
         |           "key":"int",
         |           "type":"integer",
         |           "value":24
         |        },
         |        {
         |           "key":"bool",
         |           "type":"boolean",
         |           "value":true
         |        },
         |        {
         |           "key":"blob",
         |           "type":"binary",
         |           "value":"base64:YWxpY2U="
         |        }
         |      ]
         |   },
         |   "assetOperationsMap": {
         |     "${tx.contractId.base58}": [
         |        {
         |           "operationType":"issue",
         |           "version":1,
         |           "assetId":"$assetId",
         |           "name":"Gigacoin",
         |           "description":"Gigacoin",
         |           "quantity":10000000000,
         |           "decimals":8,
         |           "isReissuable":true,
         |           "nonce":$nonce
         |        },
         |        {
         |           "operationType":"burn",
         |           "version":1,
         |           "assetId":"$assetId",
         |           "amount":1000
         |        },
         |        {
         |           "operationType":"reissue",
         |           "version":1,
         |           "assetId":"$assetId",
         |           "quantity":10000000000,
         |           "isReissuable":true
         |        },
         |        {
         |           "operationType":"transfer",
         |           "version":1,
         |           "recipient":"${recipientAccount.address}",
         |           "amount":1000
         |        }
         |      ]
         |   },
         |   "type":105,
         |   "id":"${execTx.id()}",
         |   "sender":"${senderAccount.address}",
         |   "fee":0,
         |   "timestamp":$timestamp,
         |   "resultsHash":"$resultsHash",
         |   "validationProofs":${Json.toJson(validationProofs)},
         |   "proofs":[
         |      "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
         |   ],
         |   "version":5,
         |   "readings":${readings.map(r => ReadDescriptor.format.writes(r)).mkString("[", ",", "]")},
         |   "readingsHash":"${readingsHash.hash.toString}",
         |   "outputCommitment":"${outputCommitment.hash.toString}"
         |}
         |""".stripMargin
    )

    js shouldEqual execTx.json()
  }

  property("JSON format validation with CallContractTransactionV7") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp   = System.currentTimeMillis()
    val contractId  = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val txTimestamp = timestamp - 1000
    val tx = CallContractTransactionV7
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        contractId = ByteStr.decodeBase58(contractId).get,
        params = testDataEntries,
        fee = 0,
        timestamp = txTimestamp,
        contractVersion = 1,
        feeAssetId = None,
        atomicBadge = atomicBadgeGen.generateSample().some,
        payments = List.empty,
        inputCommitment = commitmentGen().generateSample().some,
        contractEngine = "wasm",
        callFunc = Some("callFunc"),
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get
    val nonce   = Byte.MaxValue
    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val readings         = readingsGen.generateSample()
    val readingsHash     = ReadingsHash(digestSizeByteStrGen.generateSample())
    val outputCommitment = commitmentGen().generateSample()

    val dataEntryMap       = DataEntryMap(Map(tx.contractId -> testDataEntries))
    val assetOperationsMap = ContractAssetOperationMap(Map(tx.contractId -> assetOperations))

    val resultsHash = ContractTransactionValidation.resultsMapHash(
      dataEntryMap,
      assetOperationsMap
    )
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV5
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        tx,
        resultsMap = dataEntryMap,
        resultsHash,
        validationProofs,
        timestamp,
        assetOperationsMap = assetOperationsMap,
        statusCode = 0,
        errorMessage = None,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get)),
        readings = readings,
        readingsHash = readingsHash.some,
        outputCommitment = Some(outputCommitment)
      )
      .right
      .get

    val js = Json.parse(
      s"""
         |{
         |   "type":105,
         |   "id":"${execTx.id()}",
         |   "statusCode": 0,
         |   "errorMessage": null,
         |   "sender":"${senderAccount.address}",
         |   "senderPublicKey":"$senderPkBase58",
         |   "fee":0,
         |   "timestamp":$timestamp,
         |   "resultsHash":"$resultsHash",
         |   "validationProofs":${Json.toJson(validationProofs)},
         |   "proofs":[
         |      "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
         |   ],
         |   "version":5,
         |   "tx":{
         |      "type":104,
         |      "inputCommitment": "${tx.inputCommitment.get.hash.base58}",
         |      "atomicBadge": ${tx.atomicBadge.map(AtomicBadge.format.writes).getOrElse("null")},
         |      "feeAssetId": null,
         |      "id":"${tx.id()}",
         |      "sender":"${senderAccount.address}",
         |      "senderPublicKey":"$senderPkBase58",
         |      "fee":0,
         |      "contractEngine":"wasm",
         |      "contractVersion": 1,
         |      "callFunc": "callFunc",
         |      "timestamp":$txTimestamp,
         |      "proofs":[
         |         "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
         |      ],
         |      "version":7,
         |      "contractId":"$contractId",
         |      "payments": [],
         |      "params":[
         |         {
         |            "key":"int",
         |            "type":"integer",
         |            "value":24
         |         },
         |         {
         |            "key":"bool",
         |            "type":"boolean",
         |            "value":true
         |         },
         |         {
         |            "key":"blob",
         |            "type":"binary",
         |            "value":"base64:YWxpY2U="
         |         }
         |      ]
         |   },
         |   "resultsMap": {
         |     "${tx.contractId.base58}": [
         |        {
         |           "key":"int",
         |           "type":"integer",
         |           "value":24
         |        },
         |        {
         |           "key":"bool",
         |           "type":"boolean",
         |           "value":true
         |        },
         |        {
         |           "key":"blob",
         |           "type":"binary",
         |           "value":"base64:YWxpY2U="
         |        }
         |      ]
         |   },
         |   "assetOperationsMap": {
         |     "${tx.contractId.base58}": [
         |        {
         |           "operationType":"issue",
         |           "version":1,
         |           "assetId":"$assetId",
         |           "name":"Gigacoin",
         |           "description":"Gigacoin",
         |           "quantity":10000000000,
         |           "decimals":8,
         |           "isReissuable":true,
         |           "nonce":$nonce
         |        },
         |        {
         |           "operationType":"burn",
         |           "version":1,
         |           "assetId":"$assetId",
         |           "amount":1000
         |        },
         |        {
         |           "operationType":"reissue",
         |           "version":1,
         |           "assetId":"$assetId",
         |           "quantity":10000000000,
         |           "isReissuable":true
         |        },
         |        {
         |           "operationType":"transfer",
         |           "version":1,
         |           "recipient":"${recipientAccount.address}",
         |           "amount":1000
         |        }
         |      ]
         |   },
         |   "readings":${readings.map(r => ReadDescriptor.format.writes(r)).mkString("[", ",", "]")},
         |   "readingsHash":"${readingsHash.hash.toString}",
         |   "outputCommitment":"${outputCommitment.hash.toString}"
         |}
         |""".stripMargin
    )

    js shouldEqual execTx.json()
  }
}
