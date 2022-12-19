package com.wavesenterprise.transaction

import com.wavesenterprise.settings.TestFees
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.{CoreTransactionGen, crypto}
import org.scalatest.{FunSpecLike, Inside, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class CreatePolicyTransactionV1Specification extends FunSpecLike with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen with Inside {

  private val createPolicyFee = TestFees.defaultFees.forTxType(CreatePolicyTransaction.typeId)

  it("CreatePolicy Transaction encoding round trip") {
    forAll(createPolicyTransactionV1Gen()) {
      case CreatePolicyTransactionV1TestWrap(createPolicyTx, _) =>
        val encoded = createPolicyTx.bytes()

        assert(encoded.nonEmpty, "Encoded bytes are empty!")
        val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
        assert(zeroByte === 0)
        assert(encodedTypeId === CreatePolicyTransactionV1.typeId)
        assert(encodedVersion === 1)

        val decoded = CreatePolicyTransactionV1.parseTail(version = 1, encoded, 3)
        decoded.fold(ex => fail(ex), decodedTx => createPolicyTx shouldEqual decodedTx)
    }
  }

  it("CreatePolicyTransactionV1 proto serialization roundtrip") {
    forAll(createPolicyTransactionV1Gen()) {
      case CreatePolicyTransactionV1TestWrap(tx, _) =>
        val recovered = CreatePolicyTransactionV1.fromProto(tx.toInnerProto).explicitGet()
        recovered shouldEqual tx
    }
  }

  it("CreatePolicy Transaction from TransactionParser") {
    forAll(createPolicyTransactionV1Gen()) {
      case CreatePolicyTransactionV1TestWrap(createPolicyTx, _) =>
        val encoded = createPolicyTx.bytes()

        TransactionParsers
          .parseBytes(encoded)
          .fold(ex => fail(ex), tx => assert(tx.id() === createPolicyTx.id(), "Transaction ids don't match"))
    }
  }

  it("CreatePolicy Transaction's signature verifies ok") {
    forAll(createPolicyTransactionV1Gen()) {
      case CreatePolicyTransactionV1TestWrap(createPolicyTx, _) =>
        assert(crypto.verify(createPolicyTx.proofs.proofs.head.arr, createPolicyTx.bodyBytes(), createPolicyTx.sender.publicKey))
        createPolicyTx match {
          case pt: ProvenTransaction =>
            assert(crypto.verify(pt.proofs.proofs.head.arr, pt.bodyBytes(), pt.sender.publicKey))
        }
    }
  }

  it("CreatePolicy Transaction's signature doesn't corrupt after encoding trip") {
    forAll(createPolicyTransactionV1Gen()) {
      case CreatePolicyTransactionV1TestWrap(createPolicyTx, _) =>
        val signature = createPolicyTx.proofs.proofs.head.arr
        val encodedTx = createPolicyTx.bytes()

        TransactionParsers
          .parseBytes(encodedTx)
          .fold(
            ex => fail(ex),
            {
              case tx: CreatePolicyTransactionV1 =>
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
    val sender      = accountGen.sample.get
    val policyName  = "some policy name"
    val description = "some policy description"
    val timestamp   = System.currentTimeMillis()
    val proofStr    = "5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"
    val tx = CreatePolicyTransactionV1(
      sender,
      policyName,
      description,
      List.empty,
      List.empty,
      timestamp,
      createPolicyFee,
      Proofs(Seq(ByteStr.decodeBase58(proofStr).get))
    )

    val encoded                                        = tx.bytes()
    val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
    assert(zeroByte === 0)
    assert(encodedTypeId === CreatePolicyTransactionV1.typeId)
    assert(encodedVersion === 1)

    val decoded = CreatePolicyTransactionV1.parseTail(version = 1, encoded, 3)
    decoded.fold(ex => fail(ex), decodedTx => tx shouldEqual decodedTx)
  }

  it("JSON format validation") {
    val sender                 = accountGen.sample.get
    val senderPkBase58: String = Base58.encode(sender.publicKey.getEncoded)
    val policyName             = "some policy name"
    val description            = "some policy description"
    val timestamp              = System.currentTimeMillis()
    val proofStr               = "5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"
    val recipients             = severalAddressGenerator(4, 40).sample.get
    val owners                 = severalAddressGenerator(5, 21).sample.get
    val tx = CreatePolicyTransactionV1(sender,
                                       policyName,
                                       description,
                                       recipients,
                                       owners,
                                       timestamp,
                                       createPolicyFee,
                                       Proofs(Seq(ByteStr.decodeBase58(proofStr).get)))

    val recipientsStr = recipients.map(r => s""""${r.address}"""").mkString(",")
    val ownersStr     = owners.map(r => s""""${r.address}"""").mkString(",")
    val js = Json.parse(s"""{
                           |  "version": 1,
                           |  "type": 112,
                           |  "id": "${tx.id().base58}",
                           |  "sender": "${sender.address}",
                           |  "senderPublicKey": "$senderPkBase58",
                           |  "fee": $createPolicyFee,
                           |  "timestamp": $timestamp,
                           |  "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
                           |  "policyName": "$policyName",
                           |  "description": "$description",
                           |  "recipients": [$recipientsStr],
                           |  "owners": [$ownersStr]
                           |}""".stripMargin)

    js shouldEqual tx.json()
  }

  it("verify proofs") {
    forAll(createPolicyTransactionV1Gen()) {
      case CreatePolicyTransactionV1TestWrap(createPolicyTx, _) =>
        val senderPubKey = createPolicyTx.sender
        crypto.verify(createPolicyTx.proofs.proofs.head.arr, createPolicyTx.bodyBytes(), senderPubKey.publicKey)
    }
  }
}
