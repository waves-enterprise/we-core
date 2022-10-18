package com.wavesenterprise.transaction.docker

import com.wavesenterprise.NoShrink
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, IntegerDataEntry}
import com.wavesenterprise.transaction.docker.assets.{ContractAssetOperation, ContractTransferInV1}
import com.wavesenterprise.transaction.{AtomicBadge, Proofs, TransactionParsers, WithSenderAndRecipient}
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import tools.GenHelper._

class ExecutedContractTransactionV3Spec
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ContractTransactionGen
    with NoShrink
    with WithSenderAndRecipient {

  property("ExecutedContractTransactionV3 serialization roundtrip") {
    forAll(executedContractV3ParamGen) { tx: ExecutedContractTransactionV3 =>
      val recovered = ExecutedContractTransactionV3.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
    forAll(bigExecutedContractV3ParamGen) { tx: ExecutedContractTransactionV3 =>
      val recovered = ExecutedContractTransactionV3.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("ExecutedContractTransactionV3 proto serialization roundtrip") {
    forAll(executedContractV3ParamGen) { tx =>
      val recovered = ExecutedContractTransactionV3.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
    forAll(bigExecutedContractV3ParamGen) { tx =>
      val recovered = ExecutedContractTransactionV3.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("ExecutedContractTransactionV3 serialization from TypedTransaction") {
    forAll(executedContractV3ParamGen) { tx: ExecutedContractTransactionV3 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
    forAll(bigExecutedContractV3ParamGen) { tx: ExecutedContractTransactionV3 =>
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

    val nonce   = Byte.MaxValue
    val assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get

    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries, assetOperations)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV3
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get)),
        assetOperations = assetOperations
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
                       "version": 3,
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
                       ],
                       "assetOperations":
                       [
                       {
                       "operationType": "issue",
                       "version": 1,
                       "assetId": "$assetId",
                       "name": "Gigacoin",
                       "description": "Gigacoin",
                       "quantity": 10000000000,
                       "decimals": 8,
                       "isReissuable": true,
                       "nonce": $nonce
                       },
                       {
                       "operationType": "burn",
                       "version": 1,
                       "assetId": "$assetId",
                       "amount": 1000
                       },
                       {
                       "operationType": "reissue",
                       "version": 1,
                       "assetId": "$assetId",
                       "quantity": 10000000000,
                       "isReissuable": true
                       },
                       {
                       "operationType": "transfer",
                       "version": 1,
                       "recipient": "${recipientAccount.address}",
                       "amount": 1000
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

    val nonce   = Byte.MaxValue
    val assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get
    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries, assetOperations)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV3
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        assetOperations = assetOperations,
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
                       "version": 3,
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
                       ],
                       "assetOperations":
                       [
                       {
                       "operationType": "issue",
                       "version": 1,
                       "assetId": "$assetId",
                       "name": "Gigacoin",
                       "description": "Gigacoin",
                       "quantity": 10000000000,
                       "decimals": 8,
                       "isReissuable": true,
                       "nonce": $nonce
                       },
                       {
                       "operationType": "burn",
                       "version": 1,
                       "assetId": "$assetId",
                       "amount": 1000
                       },
                       {
                       "operationType": "reissue",
                       "version": 1,
                       "assetId": "$assetId",
                       "quantity": 10000000000,
                       "isReissuable": true
                       },
                       {
                       "operationType": "transfer",
                       "version": 1,
                       "recipient": "${recipientAccount.address}",
                       "amount": 1000
                       }
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }

  property("JSON format validation with CreateContractTransactionV5") {
    val params = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp          = System.currentTimeMillis()
    val txTimestamp        = timestamp - 1000
    val image              = "localhost:5000/smart-kv"
    val imageHash          = DigestUtils.sha256Hex("some_data")
    val contractName       = "contract"
    val assetId            = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get
    val validationPolicy   = ValidationPolicy.Majority
    val contractApiVersion = ContractApiVersion.`1.0`
    val payments = List(
      ContractTransferInV1(None, 100),
      ContractTransferInV1(Some(assetId), 100)
    )

    val tx = CreateContractTransactionV5
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        image = image,
        imageHash = imageHash,
        contractName = contractName,
        params = params,
        fee = 0,
        timestamp = txTimestamp,
        feeAssetId = None,
        atomicBadge = Some(AtomicBadge(Some(senderAccount.toAddress))),
        validationPolicy = validationPolicy,
        apiVersion = contractApiVersion,
        payments = payments,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val nonce = Byte.MaxValue
    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val resultsHash      = ContractTransactionValidation.resultsHash(params, assetOperations)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV3
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = params,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        assetOperations = assetOperations,
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
                       "version": 3,
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
                           "version": 5,
                           "atomicBadge": { "trustedSender": "${senderAccount.address}"},
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
                       ],
                       "assetOperations":
                       [
                       {
                       "operationType": "issue",
                       "version": 1,
                       "assetId": "$assetId",
                       "name": "Gigacoin",
                       "description": "Gigacoin",
                       "quantity": 10000000000,
                       "decimals": 8,
                       "isReissuable": true,
                       "nonce": $nonce
                       },
                       {
                       "operationType": "burn",
                       "version": 1,
                       "assetId": "$assetId",
                       "amount": 1000
                       },
                       {
                       "operationType": "reissue",
                       "version": 1,
                       "assetId": "$assetId",
                       "quantity": 10000000000,
                       "isReissuable": true
                       },
                       {
                       "operationType": "transfer",
                       "version": 1,
                       "recipient": "${recipientAccount.address}",
                       "amount": 1000
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

    val assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get
    val nonce   = Byte.MaxValue
    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries, assetOperations)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV3
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        tx,
        results = testDataEntries,
        resultsHash,
        validationProofs,
        timestamp,
        assetOperations,
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
                       "version": 3,
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
                       ],
                       "assetOperations":
                       [
                       {
                       "operationType": "issue",
                       "version": 1,
                       "assetId": "$assetId",
                       "name": "Gigacoin",
                       "description": "Gigacoin",
                       "quantity": 10000000000,
                       "decimals": 8,
                       "isReissuable": true,
                       "nonce": $nonce
                       },
                       {
                       "operationType": "burn",
                       "version": 1,
                       "assetId": "$assetId",
                       "amount": 1000
                       },
                       {
                       "operationType": "reissue",
                       "version": 1,
                       "assetId": "$assetId",
                       "quantity": 10000000000,
                       "isReissuable": true
                       },
                       {
                       "operationType": "transfer",
                       "version": 1,
                       "recipient": "${recipientAccount.address}",
                       "amount": 1000
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

    val assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get
    val nonce   = Byte.MaxValue
    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV3
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        assetOperations = assetOperations,
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
                       "version": 3,
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
                       ],
                       "assetOperations":
                       [
                       {
                       "operationType": "issue",
                       "version": 1,
                       "assetId": "$assetId",
                       "name": "Gigacoin",
                       "description": "Gigacoin",
                       "quantity": 10000000000,
                       "decimals": 8,
                       "isReissuable": true,
                       "nonce": $nonce
                       },
                       {
                       "operationType": "burn",
                       "version": 1,
                       "assetId": "$assetId",
                       "amount": 1000
                       },
                       {
                       "operationType": "reissue",
                       "version": 1,
                       "assetId": "$assetId",
                       "quantity": 10000000000,
                       "isReissuable": true
                       },
                       {
                       "operationType": "transfer",
                       "version": 1,
                       "recipient": "${recipientAccount.address}",
                       "amount": 1000
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

    val assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get
    val nonce   = Byte.MaxValue
    val assetOperations = List(
      ContractAssetOperation.ContractIssueV1(assetId, "Gigacoin", "Gigacoin", 10000000000L, 8, true, nonce),
      ContractAssetOperation.ContractBurnV1(Option(assetId), 1000),
      ContractAssetOperation.ContractReissueV1(assetId, 10000000000L, true),
      ContractAssetOperation.ContractTransferOutV1(None, recipientAccount.toAddress, 1000),
    )

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries, assetOperations)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV3
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        assetOperations = assetOperations,
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
                       "version": 3,
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
                       ],
                       "assetOperations":
                       [
                       {
                       "operationType": "issue",
                       "version": 1,
                       "assetId": "$assetId",
                       "name": "Gigacoin",
                       "description": "Gigacoin",
                       "quantity": 10000000000,
                       "decimals": 8,
                       "isReissuable": true,
                       "nonce": $nonce
                       },
                       {
                       "operationType": "burn",
                       "version": 1,
                       "assetId": "$assetId",
                       "amount": 1000
                       },
                       {
                       "operationType": "reissue",
                       "version": 1,
                       "assetId": "$assetId",
                       "quantity": 10000000000,
                       "isReissuable": true
                       },
                       {
                       "operationType": "transfer",
                       "version": 1,
                       "recipient": "${recipientAccount.address}",
                       "amount": 1000
                       }
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }

  property("JSON format validation with CallContractTransactionV5") {
    val testDataEntries = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp       = System.currentTimeMillis()
    val txTimestamp     = timestamp - 1000
    val contractId      = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val contractVersion = 1
    val feeAssetId      = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val payments = List(
      ContractTransferInV1(None, 100),
      ContractTransferInV1(Some(ByteStr.decodeBase58(feeAssetId).get), 100)
    )

    val tx = CallContractTransactionV5
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        testDataEntries,
        fee = 0,
        txTimestamp,
        contractVersion,
        Some(ByteStr.decodeBase58(feeAssetId).get),
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        payments,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
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

    val resultsHash      = ContractTransactionValidation.resultsHash(testDataEntries, assetOperations)
    val validationProofs = validationProofsGen(resultsHash).generateSample()

    val execTx = ExecutedContractTransactionV3
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        tx = tx,
        results = testDataEntries,
        resultsHash = resultsHash,
        timestamp = timestamp,
        validationProofs = validationProofs,
        assetOperations = assetOperations,
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
                       "version": 3,
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
                           "version": 5,
                           "atomicBadge": { "trustedSender": "${senderAccount.address}"},
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
                           ],
                           "payments":
                           [
                           {
                           "amount": 100
                           },
                           {
                           "assetId": "$feeAssetId",
                           "amount": 100
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
                       ],
                       "assetOperations":
                       [
                       {
                       "operationType": "issue",
                       "version": 1,
                       "assetId": "$assetId",
                       "name": "Gigacoin",
                       "description": "Gigacoin",
                       "quantity": 10000000000,
                       "decimals": 8,
                       "isReissuable": true,
                       "nonce": $nonce
                       },
                       {
                       "operationType": "burn",
                       "version": 1,
                       "assetId": "$assetId",
                       "amount": 1000
                       },
                       {
                       "operationType": "reissue",
                       "version": 1,
                       "assetId": "$assetId",
                       "quantity": 10000000000,
                       "isReissuable": true
                       },
                       {
                       "operationType": "transfer",
                       "version": 1,
                       "recipient": "${recipientAccount.address}",
                       "amount": 1000
                       }
                       ]
                       }
  """)

    js shouldEqual execTx.json()
  }
}
