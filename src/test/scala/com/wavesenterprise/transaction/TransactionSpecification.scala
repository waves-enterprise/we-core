package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesenterprise.transaction.transfer._

class TransactionSpecification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen {

  property("transaction fields should be constructed in a right way") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>
        val sender    = accountGen.sample.get
        val recipient = accountGen.sample.get

        val tx = createWestTransfer(sender, recipient.toAddress, amount, fee, time).explicitGet()

        tx.timestamp shouldEqual time
        tx.amount shouldEqual amount
        tx.fee shouldEqual fee
        tx.sender shouldEqual sender
        tx.recipient.stringRepr shouldEqual recipient.address
    }
  }

  property("bytes()/parse() roundtrip should preserve a transaction") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>
        val sender    = accountGen.sample.get
        val recipient = accountGen.sample.get
        val tx        = createWestTransfer(sender, recipient.toAddress, amount, fee, time).explicitGet()
        val txAfter   = TransferTransactionV2.parseBytes(tx.bytes()).get

        txAfter.getClass.shouldBe(tx.getClass)
        tx.proofs.proofs.map(_.base58) should contain theSameElementsAs txAfter.proofs.proofs.map(_.base58)
        tx.sender shouldEqual txAfter.asInstanceOf[TransferTransactionV2].sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

  property("TransferTransaction should deserialize to LagonakiTransaction") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>
        val sender    = accountGen.sample.get
        val recipient = accountGen.sample.get
        val tx        = createWestTransfer(sender, recipient.toAddress, amount, fee, time).explicitGet()
        val txAfter   = TransactionParsers.parseBytes(tx.bytes()).get.asInstanceOf[TransferTransactionV2]

        txAfter.getClass.shouldBe(tx.getClass)

        tx.proofs.proofs.map(_.base58) should contain theSameElementsAs txAfter.proofs.proofs.map(_.base58)
        tx.sender shouldEqual txAfter.sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

}
