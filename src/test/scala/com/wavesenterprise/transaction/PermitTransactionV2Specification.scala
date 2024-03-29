package com.wavesenterprise.transaction

import com.wavesenterprise.transaction.acl.{PermitTransaction, PermitTransactionV2}
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.{CoreTransactionGen, crypto}
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PermitTransactionV2Specification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen {
  property("Permit Transaction encoding round trip") {
    forAll(permitTransactionV2Gen()) { permTx =>
      val encoded = permTx.bytes()

      assert(encoded.nonEmpty, "Encoded bytes are empty!")
      val Array(zeroByte, encodedTypeId, encodedVersion) = encoded.take(3)
      assert(zeroByte === 0)
      assert(encodedTypeId === PermitTransaction.typeId)
      assert(encodedVersion === 2)

      val decoded = PermitTransactionV2.parseTail(version = 2, encoded, 3)
      decoded.fold(ex => fail(ex), _ => succeed)
    }
  }

  property("PermitTransactionV1 proto serialization roundtrip") {
    forAll(permitTransactionV2Gen()) { tx =>
      val recovered = PermitTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("Permit Transaction from TransactionParser") {
    forAll(permitTransactionV2Gen()) { permTx =>
      val encoded = permTx.bytes()

      TransactionParsers
        .parseBytes(encoded)
        .fold(ex => fail(ex), tx => assert(tx.id() === permTx.id(), "Transaction ids don't match"))
    }
  }

  property("Permit Transaction's signature verifies ok") {
    forAll(permitTransactionV2Gen()) { permTx =>
      assert(crypto.verify(permTx.proofs.proofs.head.arr, permTx.bodyBytes(), permTx.sender.publicKey))
      permTx match {
        case pt: ProvenTransaction =>
          assert(crypto.verify(pt.proofs.proofs.head.arr, pt.bodyBytes(), pt.sender.publicKey))
      }
    }
  }

  property("Permit Transaction's signature doesn't corrupt after encoding trip") {
    forAll(permitTransactionV2Gen()) { permTx =>
      val signature = permTx.proofs.proofs.head.arr
      val encodedTx = permTx.bytes()

      TransactionParsers
        .parseBytes(encodedTx)
        .fold(
          ex => fail(ex),
          {
            case tx: PermitTransaction =>
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

  property("Permit Transaction constructor rejects different timestamps for tx and PermissionOp") {
    forAll(accountGen, permitTransactionV2Gen(), timestampGen, timestampGen) { (signer, generatedTx, txTimestamp, permOpTimestamp) =>
      whenever(txTimestamp != permOpTimestamp) {
        PermitTransactionV2.selfSigned(signer,
                                       generatedTx.target,
                                       txTimestamp,
                                       generatedTx.fee,
                                       generatedTx.permissionOp.copy(timestamp = permOpTimestamp),
                                       generatedTx.atomicBadge) shouldBe 'left
      }
    }
  }
}
