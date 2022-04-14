package com.wavesenterprise.transaction

import com.wavesenterprise.acl.PermissionsGen
import com.wavesenterprise.settings.TestFees
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.{CoreTransactionGen, crypto}
import org.scalatest.{FunSpecLike, Inside, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class UpdatePolicyTransactionV1Specification extends FunSpecLike with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen with Inside {

  private val defaultPolicyId: ByteStr = ByteStr(Base58.decode("thfgsdfewe").get)
  private val updatePolicyFee          = TestFees.defaultFees.forTxType(CreatePolicyTransactionV1.typeId)

  it("UpdatePolicy Transaction encoding round trip") {
    forAll(updatePolicyTransactionV1Gen()) { updatePolicyTx =>
      val encoded = updatePolicyTx.bytes()

      assert(encoded.nonEmpty, "Encoded bytes are empty!")
      val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
      assert(zeroByte === 0)
      assert(encodedTypeId === UpdatePolicyTransactionV1.typeId)
      assert(encodedVersion === 1)

      val decoded = UpdatePolicyTransactionV1.parseTail(version = 1, encoded, 3)
      decoded.fold(ex => fail(ex), decodedTx => updatePolicyTx shouldEqual decodedTx)
    }
  }

  it("UpdatePolicyTransactionV1 proto serialization roundtrip") {
    forAll(updatePolicyTransactionV1Gen()) { tx =>
      val recovered = UpdatePolicyTransactionV1.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  it("serialize and deserialize UpdatePolicy Transaction with empty owners and recipients") {
    val updatePolicyTx = UpdatePolicyTransactionV1(
      sender = accountGen.sample.get,
      policyId = defaultPolicyId,
      recipients = List.empty,
      owners = List.empty,
      opType = PermissionsGen.permissionOpTypeGen.sample.get,
      timestamp = System.currentTimeMillis(),
      fee = updatePolicyFee,
      proofs = Proofs.empty
    )

    val encoded                                        = updatePolicyTx.bytes()
    val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
    assert(zeroByte === 0)
    assert(encodedTypeId === UpdatePolicyTransactionV1.typeId)
    assert(encodedVersion === 1)

    val decoded = UpdatePolicyTransactionV1.parseTail(version = 1, encoded, 3)
    decoded.fold(ex => fail(ex), decodedTx => updatePolicyTx shouldEqual decodedTx)
    decoded.get.owners shouldBe empty
    decoded.get.recipients shouldBe empty
  }

  it("UpdatePolicy Transaction from TransactionParser") {
    forAll(updatePolicyTransactionV1Gen()) { updatePolicyTx =>
      val encoded = updatePolicyTx.bytes()

      TransactionParsers
        .parseBytes(encoded)
        .fold(ex => fail(ex), tx => assert(tx.id() === updatePolicyTx.id(), "Transaction ids don't match"))
    }
  }

  it("UpdatePolicy Transaction's signature verifies ok") {
    forAll(updatePolicyTransactionV1Gen()) { updatePolicyTx =>
      assert(crypto.verify(updatePolicyTx.proofs.proofs.head.arr, updatePolicyTx.bodyBytes(), updatePolicyTx.sender.publicKey))
      updatePolicyTx match {
        case pt: ProvenTransaction =>
          assert(crypto.verify(pt.proofs.proofs.head.arr, pt.bodyBytes(), pt.sender.publicKey))
      }
    }
  }

  it("UpdatePolicy Transaction's signature doesn't corrupt after encoding trip") {
    forAll(updatePolicyTransactionV1Gen()) { updatePolicyTx =>
      val signature = updatePolicyTx.proofs.proofs.head.arr
      val encodedTx = updatePolicyTx.bytes()

      TransactionParsers
        .parseBytes(encodedTx)
        .fold(
          ex => fail(ex), {
            case tx: UpdatePolicyTransactionV1 =>
              val parsedSignature = tx.proofs.proofs.head.arr
              assert(signature.length === parsedSignature.length, "Signature lengths should match")

              assert(
                parsedSignature.sameElements(signature),
                s"Signatures in proof don't match! Got: [${Base58.encode(parsedSignature)}], expected [${Base58.encode(signature)}]"
              )

              assert(crypto.verify(parsedSignature, tx.bodyBytes(), tx.sender.publicKey), "Signature is invalid")
          }
        )
    }
  }

  it("Success serialize and deserialize with empty lists") {
    val sender    = accountGen.sample.get
    val timestamp = System.currentTimeMillis()
    val proofStr  = "5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"
    val tx = UpdatePolicyTransactionV1(
      sender = sender,
      policyId = defaultPolicyId,
      recipients = List.empty,
      owners = List.empty,
      opType = PermissionsGen.permissionOpTypeGen.sample.get,
      timestamp = timestamp,
      fee = updatePolicyFee,
      proofs = Proofs(Seq(ByteStr.decodeBase58(proofStr).get))
    )

    val encoded                                        = tx.bytes()
    val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
    assert(zeroByte === 0)
    assert(encodedTypeId === UpdatePolicyTransactionV1.typeId)
    assert(encodedVersion === 1)

    val decoded = UpdatePolicyTransactionV1.parseTail(version = 1, encoded, 3)
    decoded.fold(ex => fail(ex), decodedTx => tx shouldEqual decodedTx)
  }

  it("JSON format validation") {
    val sender                 = accountGen.sample.get
    val senderPkBase58: String = Base58.encode(sender.publicKey.getEncoded)
    val timestamp              = System.currentTimeMillis()
    val proofStr               = "5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"
    val recipients             = severalAddressGenerator(4, 40).sample.get
    val owners                 = severalAddressGenerator(5, 21).sample.get
    val opType                 = PermissionsGen.permissionOpTypeGen.sample.get
    val tx = UpdatePolicyTransactionV1(sender,
                                       defaultPolicyId,
                                       recipients,
                                       owners,
                                       opType,
                                       timestamp,
                                       updatePolicyFee,
                                       Proofs(Seq(ByteStr.decodeBase58(proofStr).get)))

    val recipientsStr = recipients.map(r => s""""${r.address}"""").mkString(",")
    val ownersStr     = owners.map(r => s""""${r.address}"""").mkString(",")
    val js            = Json.parse(s"""{
                                        |  "version": 1,
                                        |  "type": 113,
                                        |  "id": "${tx.id().base58}",
                                        |  "sender": "${sender.address}",
                                        |  "senderPublicKey": "$senderPkBase58",
                                        |  "fee": $updatePolicyFee,
                                        |  "timestamp": $timestamp,
                                        |  "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
                                        |  "policyId": "${Base58.encode(defaultPolicyId.arr)}",
                                        |  "recipients": [$recipientsStr],
                                        |  "opType": "${opType.str}",
                                        |  "owners": [$ownersStr]
                                        |}""".stripMargin)

    js shouldEqual tx.json()
  }

  it("verify proofs") {
    forAll(updatePolicyTransactionV1Gen()) { updatePolicyTx =>
      val senderPubKey = updatePolicyTx.sender
      crypto.verify(updatePolicyTx.proofs.proofs.head.arr, updatePolicyTx.bodyBytes(), senderPubKey.publicKey)
    }
  }
}
