package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, IntegerDataEntry, StringDataEntry}
import com.wavesenterprise.transaction.{Proofs, TransactionParsers, ValidationError}
import com.wavesenterprise.utils.Base64
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import com.wavesenterprise.utils.EitherUtils.EitherExt

class CallContractTransactionV1Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("CallContractTransactionV1Spec serialization roundtrip") {
    forAll(callContractV1ParamGen) { tx: CallContractTransactionV1 =>
      val recovered = CallContractTransactionV1.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("CallContractTransactionV1 proto serialization roundtrip") {
    forAll(callContractV1ParamGen) { tx =>
      val recovered = CallContractTransactionV1.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("CallContractTransactionV1Spec serialization from TypedTransaction") {
    forAll(callContractV1ParamGen) { tx: CallContractTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("CallContractTransactionV1Spec negative validation cases") {
    forAll(callContractV1ParamGen) {
      case CallContractTransactionV1(sender, contractId, _, fee, timestamp, proofs) =>
        val emptyKeyParams = List(IntegerDataEntry("", 2))
        val emptyKeyEither =
          CallContractTransactionV1.create(sender, contractId, emptyKeyParams, fee, timestamp, proofs)
        emptyKeyEither shouldBe Left(ValidationError.GenericError("Param with empty key was found"))

        val duplicateKeysParams = List(IntegerDataEntry("key1", 2), StringDataEntry("key1", "value"))
        val duplicateKeysEither =
          CallContractTransactionV1.create(sender, contractId, duplicateKeysParams, fee, timestamp, proofs)
        duplicateKeysEither shouldBe Left(ValidationError.GenericError("Params with duplicate keys were found"))

        val tooBigTxParams = List(BinaryDataEntry("key1", ByteStr(Array.fill(ExecutableTransaction.MaxBytes)(1: Byte))))
        val tooBigTxEither =
          CallContractTransactionV1.create(sender, contractId, tooBigTxParams, fee, timestamp, proofs)
        tooBigTxEither.left.get shouldBe a[ValidationError.ContractTransactionTooBig]

        val withNonAsciiKeysParams = List(IntegerDataEntry("key∂√1", 2), StringDataEntry("kååey1", "value"))
        val withNonAsciiKeysEither =
          CallContractTransactionV1.create(sender, contractId, withNonAsciiKeysParams, fee, timestamp, proofs)
        withNonAsciiKeysEither shouldBe Left(ValidationError.InvalidContractKeys("key∂√1 -> ∂√; kååey1 -> å"))
    }
  }

  property("JSON format validation") {
    val params = List(
      IntegerDataEntry("int", 24),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    )
    val timestamp  = System.currentTimeMillis()
    val contractId = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val tx = CallContractTransactionV1
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58(contractId).get,
        params,
        fee = 0,
        timestamp,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 104,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
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
                       }
  """)

    js shouldEqual tx.json()
  }

}
