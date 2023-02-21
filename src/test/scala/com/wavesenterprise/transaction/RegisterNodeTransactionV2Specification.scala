package com.wavesenterprise.transaction

import com.wavesenterprise.acl.OpType
import com.wavesenterprise.settings.TestFees
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.validation.RegisterNodeValidation
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.{CoreTransactionGen, crypto}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class RegisterNodeTransactionV2Specification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen {
  val regNodeTxFee: Long = TestFees.defaultFees.forTxType(RegisterNodeTransactionV2.typeId)

  property("RegisterNode Transaction encoding round trip") {
    forAll(registerNodeTransactionV2Gen()) { regNodeTx =>
      val encoded = regNodeTx.bytes()

      assert(encoded.nonEmpty, "Encoded bytes are empty!")
      val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
      assert(zeroByte === 0)
      assert(encodedTypeId === RegisterNodeTransactionV2.typeId)
      assert(encodedVersion === 2)

      val decoded = RegisterNodeTransactionV2.parseTail(version = 2, encoded, 3)
      decoded.fold(ex => fail(ex), decodedTx => regNodeTx shouldEqual decodedTx)
    }
  }

  property("RegisterNode proto serialization roundtrip") {
    forAll(registerNodeTransactionV2Gen()) { tx =>
      val recovered = RegisterNodeTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("RegisterNode Transaction from TransactionParser") {
    forAll(registerNodeTransactionV2Gen()) { regNodeTx =>
      val encoded = regNodeTx.bytes()

      TransactionParsers
        .parseBytes(encoded)
        .fold(ex => fail(ex), tx => assert(tx.id() === regNodeTx.id(), "Transaction ids don't match"))
    }
  }

  property("RegisterNode Transaction's signature verifies ok") {
    forAll(registerNodeTransactionV2Gen()) { regNodeTx =>
      assert(crypto.verify(regNodeTx.proofs.proofs.head.arr, regNodeTx.bodyBytes(), regNodeTx.sender.publicKey))
      regNodeTx match {
        case pt: ProvenTransaction =>
          assert(crypto.verify(pt.proofs.proofs.head.arr, pt.bodyBytes(), pt.sender.publicKey))
      }
    }
  }

  property("RegisterNode Transaction's signature doesn't corrupt after encoding trip") {
    forAll(registerNodeTransactionV2Gen()) { regNodeTx =>
      val signature = regNodeTx.proofs.proofs.head.arr
      val encodedTx = regNodeTx.bytes()

      TransactionParsers
        .parseBytes(encodedTx)
        .fold(
          ex => fail(ex),
          {
            case tx: RegisterNodeTransactionV2 =>
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

  property("RegisterNode tx doesn't allow empty nodeName for add operation type") {
    forAll(accountGen, ntpTimestampGen) {
      case (senderAcc, timestamp) =>
        val createResult = RegisterNodeTransactionV2.selfSigned(senderAcc,
                                                                senderAcc,
                                                                None,
                                                                OpType.Add,
                                                                timestamp,
                                                                regNodeTxFee,
                                                                Some(AtomicBadge(Some(senderAcc.toAddress))))
        createResult.left.toString should include("NodeName should not be empty")
    }
  }

  property("RegisterNode tx does allow empty nodeName for remove operation type") {
    forAll(accountGen, ntpTimestampGen) {
      case (senderAcc, timestamp) =>
        val createResult = RegisterNodeTransactionV2.selfSigned(senderAcc,
                                                                senderAcc,
                                                                None,
                                                                OpType.Remove,
                                                                timestamp,
                                                                regNodeTxFee,
                                                                Some(AtomicBadge(Some(senderAcc.toAddress))))
        createResult shouldBe 'right
    }
  }

  property("RegisterNode tx doesn't allow nodeNames longer than Short.MaxValue serialized") {
    val genNodeNameBiggerThanAllowed = for {
      length   <- Gen.chooseNum(RegisterNodeValidation.NodeNameMaxLength, 1000)
      baseChar <- Gen.alphaNumChar
    } yield Some(baseChar.toString * length)

    forAll(accountGen, ntpTimestampGen, genNodeNameBiggerThanAllowed) {
      case (senderAcc, timestamp, tooLongNodeName) =>
        val createResult = RegisterNodeTransactionV2.selfSigned(senderAcc,
                                                                senderAcc,
                                                                tooLongNodeName,
                                                                OpType.Add,
                                                                timestamp,
                                                                regNodeTxFee,
                                                                Some(AtomicBadge(Some(senderAcc.toAddress))))
        createResult.left.toString should include(s"NodeName length should be less than ${RegisterNodeValidation.NodeNameMaxLength}")
    }
  }

  property("JSON format validation") {
    val sender                 = accountGen.sample.get
    val senderPkBase58: String = Base58.encode(sender.publicKey.getEncoded)
    val target                 = accountGen.sample.get
    val nodeName               = "some-node-name"
    val opType                 = OpType.Add
    val timestamp              = System.currentTimeMillis()
    val proofStr               = "5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"
    val tx = RegisterNodeTransactionV2(
      sender = sender,
      target = target,
      nodeName = Some(nodeName),
      opType = opType,
      timestamp = timestamp,
      fee = regNodeTxFee,
      Some(AtomicBadge(Some(sender.toAddress))),
      proofs = Proofs(Seq(ByteStr.decodeBase58(proofStr).get))
    )

    val js = Json.parse(s"""{
                           |  "type": 111,
                           |  "id": "${tx.id()}",
                           |  "version": 2,
                           |  "sender": "${sender.address}",
                           |  "senderPublicKey": "$senderPkBase58",
                           |  "fee": $regNodeTxFee,
                           |  "atomicBadge": { "trustedSender": "${sender.address}"},
                           |  "timestamp": $timestamp,
                           |  "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
                           |  "nodeName": "$nodeName",
                           |  "opType": "${opType.str}",
                           |  "target": "${target.address}",
                           |  "targetPubKey": "${target.publicKeyBase58}"
                           |}""".stripMargin)

    js shouldEqual tx.json()
  }
}
