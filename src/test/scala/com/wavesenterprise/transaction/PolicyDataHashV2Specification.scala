package com.wavesenterprise.transaction

import com.wavesenterprise.crypto.SignatureLength
import com.wavesenterprise.privacy.PolicyDataHash
import com.wavesenterprise.settings.TestFees
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.{CoreTransactionGen, crypto}
import org.scalatest.{FunSpecLike, Inside, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

import scala.util.Random

class PolicyDataHashV2Specification extends FunSpecLike with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen with Inside {

  private val defaultPolicyId: ByteStr       = ByteStr(Base58.decode("thfgsdfewe").get)
  private val policyDataHash: PolicyDataHash = PolicyDataHash.fromDataBytes(byteArrayGen(66000).sample.get)
  private val policyDataHashFee              = TestFees.defaultFees.forTxType(PolicyDataHashTransactionV2.typeId)

  it("PolicyDataHash Transaction encoding round trip") {
    forAll(policyDataHashTransactionV2Gen()) {
      case PolicyDataWithTxV2(_, policyDataHashTx) =>
        val encoded = policyDataHashTx.bytes()

        assert(encoded.nonEmpty, "Encoded bytes are empty!")
        val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
        assert(zeroByte === 0)
        assert(encodedTypeId === PolicyDataHashTransactionV2.typeId)
        assert(encodedVersion === 2)

        val decoded = PolicyDataHashTransactionV2.parseTail(version = 1, encoded, 3)
        decoded.fold(ex => fail(ex), decodedTx => policyDataHashTx shouldEqual decodedTx)
    }
  }

  it("PolicyDataHashTransactionV2 proto serialization roundtrip") {
    forAll(policyDataHashTransactionV2Gen()) {
      case PolicyDataWithTxV2(_, tx) =>
        val recovered = PolicyDataHashTransactionV2.fromProto(tx.toInnerProto).explicitGet()
        recovered shouldEqual tx
    }
  }

  it("PolicyDataHash Transaction from TransactionParser") {
    forAll(policyDataHashTransactionV2Gen()) {
      case PolicyDataWithTxV2(_, policyDataHashTx) =>
        val encoded = policyDataHashTx.bytes()

        TransactionParsers
          .parseBytes(encoded)
          .fold(ex => fail(ex), tx => assert(tx.id() === policyDataHashTx.id(), "Transaction ids don't match"))
    }
  }

  it("PolicyDataHash Transaction's signature verifies ok") {
    forAll(policyDataHashTransactionV2Gen()) {
      case PolicyDataWithTxV2(_, policyDataHashTx) =>
        assert(crypto.verify(policyDataHashTx.proofs.proofs.head.arr, policyDataHashTx.bodyBytes(), policyDataHashTx.sender.publicKey))
        policyDataHashTx match {
          case pt: ProvenTransaction =>
            assert(crypto.verify(pt.proofs.proofs.head.arr, pt.bodyBytes(), pt.sender.publicKey))
        }
    }
  }

  it("PolicyDataHash Transaction's signature doesn't corrupt after encoding trip") {
    forAll(policyDataHashTransactionV2Gen()) {
      case PolicyDataWithTxV2(_, policyDataHashTx) =>
        val signature = policyDataHashTx.proofs.proofs.head.arr
        val encodedTx = policyDataHashTx.bytes()

        TransactionParsers
          .parseBytes(encodedTx)
          .fold(
            ex => fail(ex),
            {
              case tx: PolicyDataHashTransactionV2 =>
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

  it("JSON format validation") {
    val sender                 = accountGen.sample.get
    val senderPkBase58: String = Base58.encode(sender.publicKey.getEncoded)
    val timestamp              = System.currentTimeMillis()
    val proofStr               = "5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"

    val tx = PolicyDataHashTransactionV2(sender,
                                         policyDataHash,
                                         defaultPolicyId,
                                         timestamp,
                                         policyDataHashFee,
                                         None,
                                         Proofs(Seq(ByteStr.decodeBase58(proofStr).get)))

    val js = Json.parse(s"""{
                                        |  "version": 2,
                                        |  "type": 114,
                                        |  "id": "${tx.id().base58}",
                                        |  "sender": "${sender.address}",
                                        |  "senderPublicKey": "$senderPkBase58",
                                        |  "fee": $policyDataHashFee,
                                        |  "feeAssetId": null,
                                        |  "timestamp": $timestamp,
                                        |  "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
                                        |  "policyId": "${Base58.encode(defaultPolicyId.arr)}",
                                        |  "dataHash": "${tx.dataHash.stringRepr}"
                                        |}""".stripMargin)

    js shouldEqual tx.json()
  }

  it("verify proofs") {
    forAll(policyDataHashTransactionV2Gen()) {
      case PolicyDataWithTxV2(_, policyDataHashTx) =>
        val senderPubKey = policyDataHashTx.sender
        crypto.verify(policyDataHashTx.proofs.proofs.head.arr, policyDataHashTx.bodyBytes(), senderPubKey.publicKey)
    }
  }

  def badProof(): Proofs = {
    val bidSign = ByteStr((1 to SignatureLength).map(_ => Random.nextPrintableChar().toByte).toArray)
    Proofs(Seq(bidSign))
  }
}
