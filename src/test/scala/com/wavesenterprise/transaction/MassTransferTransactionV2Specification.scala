package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.transaction.transfer._
import com.wavesenterprise.transaction.validation.TransferValidation
import com.wavesenterprise.transaction.validation.TransferValidation.MaxTransferCount
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MassTransferTransactionV2Specification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen {

  property("serialization roundtrip") {
    forAll(massTransferV2Gen()) { tx: MassTransferTransactionV2 =>
      require(tx.bytes().head == (0: Byte))
      require(tx.bytes()(1) == MassTransferTransactionV2.typeId)
      val recovered = MassTransferTransactionV2.parseBytes(tx.bytes()).get

      recovered.sender.address shouldEqual tx.sender.address
      recovered.assetId.map(_ == tx.assetId.get).getOrElse(tx.assetId.isEmpty) shouldBe true
      recovered.feeAssetId.map(_ == tx.feeAssetId.get).getOrElse(tx.feeAssetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual tx.timestamp
      recovered.fee shouldEqual tx.fee

      recovered.transfers.zip(tx.transfers).foreach {
        case (ParsedTransfer(rr, ra), ParsedTransfer(tr, ta)) =>
          rr shouldEqual tr
          ra shouldEqual ta
      }

      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("MassTransferTransactionV1 proto serialization roundtrip") {
    forAll(massTransferV2Gen()) { tx =>
      val recovered = MassTransferTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("serialization from TypedTransaction") {
    forAll(massTransferV2Gen()) { tx: MassTransferTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("property validation") {
    import MassTransferTransactionV2.create

    val badVersionGen = Arbitrary.arbByte.arbitrary.filter(x => !MassTransferTransactionV2.supportedVersions.contains(x))
    forAll(massTransferV2Gen(), badVersionGen) {
      case (MassTransferTransactionV2(sender, assetId, transfers, timestamp, fee, attachment, feeAssetId, proofs), badVersion) =>
        val tooManyTransfers   = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, 1L))
        val tooManyTransfersEi = create(sender, assetId, tooManyTransfers, timestamp, fee, attachment, feeAssetId, proofs)
        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers ${tooManyTransfers.length} is greater than $MaxTransferCount"))

        val negativeTransfer   = List(ParsedTransfer(sender.toAddress, -1L))
        val negativeTransferEi = create(sender, assetId, negativeTransfer, timestamp, fee, attachment, feeAssetId, proofs)
        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))

        val oneHalf    = Long.MaxValue / 2 + 1
        val overflow   = List.fill(2)(ParsedTransfer(sender.toAddress, oneHalf))
        val overflowEi = create(sender, assetId, overflow, timestamp, fee, attachment, feeAssetId, proofs)
        overflowEi shouldBe Left(ValidationError.OverflowError)

        val feeOverflow   = List(ParsedTransfer(sender.toAddress, oneHalf))
        val feeOverflowEi = create(sender, assetId, feeOverflow, timestamp, oneHalf, attachment, feeAssetId, proofs)
        feeOverflowEi shouldBe Left(ValidationError.OverflowError)

        val longAttachment   = Array.fill(TransferValidation.MaxAttachmentSize + 1)(1: Byte)
        val longAttachmentEi = create(sender, assetId, transfers, timestamp, fee, longAttachment, feeAssetId, proofs)
        longAttachmentEi shouldBe Left(ValidationError.TooBigArray)
    }
  }
}
