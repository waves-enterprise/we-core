package com.wavesenterprise.transaction.wasm

import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.lang.EitherExt
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.docker.{ContractTransactionGen, UpdateContractTransactionV6}
import com.wavesenterprise.transaction.{Proofs, TransactionParsers}
import com.wavesenterprise.utils.Base64
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets.UTF_8

class UpdateContractTransactionV6Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("UpdateContractTransactionV6Spec serialization roundtrip") {
    forAll(updateContractV6ParamGen()) { tx =>
      val recovered = UpdateContractTransactionV6.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("UpdateContractTransactionV6 proto serialization roundtrip") {
    forAll(updateContractV6ParamGen()) { tx =>
      val recovered = UpdateContractTransactionV6.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldBe tx
    }
  }

  property("UpdateContractTransactionV6Spec serialization from TypedTransaction") {
    forAll(updateContractV6ParamGen()) { tx =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldBe tx
    }
  }

  property("JSON format validation") {
    val timestamp    = System.currentTimeMillis()
    val contractId   = "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz"
    val bytecodeStr  = "some_data"
    val bytecodeHash = DigestUtils.sha256Hex(bytecodeStr)
    val tx = UpdateContractTransactionV6
      .create(
        sender = PublicKeyAccount(senderAccount.publicKey),
        contractId = ByteStr.decodeBase58(contractId).get,
        fee = 0,
        timestamp = timestamp,
        feeAssetId = None,
        atomicBadge = None,
        validationPolicy = ValidationPolicy.Any,
        groupParticipants = Set.empty,
        groupOwners = Set.empty,
        bytecode = bytecodeStr.getBytes(UTF_8),
        bytecodeHash = bytecodeHash,
        proofs = Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 107,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "timestamp": $timestamp,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 6,
                       "bytecode": "${Base64.encode(bytecodeStr.getBytes(UTF_8))}",
                       "contractId": "$contractId",
                       "bytecodeHash": "$bytecodeHash",
                       "atomicBadge": null,
                       "validationPolicy": {"type":"any"},
                       "groupParticipants": [],
                       "groupOwners": [],
                       "feeAssetId": null
                       }
  """)

    js shouldEqual tx.json()
  }
}
