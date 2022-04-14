package com.wavesenterprise.transaction

import cats.implicits._
import cats.{Order => _}
import com.wavesenterprise.account.AddressScheme
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.transaction.transfer.ParsedTransfer

package object validation {
  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8
  val MaxTransferCount     = 100
  val MaxEntryCount        = 100

  def validateFee(fee: Long): Either[ValidationError, Long] = {
    Either
      .cond(
        fee >= 0,
        fee,
        ValidationError.InsufficientFee()
      )
  }

  def validatePositiveAmount(amount: Long, of: => String): Either[ValidationError, Long] = {
    Either
      .cond(
        amount > 0,
        amount,
        ValidationError.NegativeAmount(amount, of)
      )
  }

  def validateNonNegativeAmount(amount: Long, of: => String): Either[ValidationError, Long] = {
    Either
      .cond(
        amount >= 0,
        amount,
        ValidationError.NegativeAmount(amount, of)
      )
  }

  def validateMassTransfers(transfers: List[ParsedTransfer], fee: Long): Either[ValidationError, Unit] = {
    def checkOverflow: Either[ValidationError, Long] =
      Either
        .catchNonFatal(
          transfers.map(_.amount).fold(fee)(Math.addExact)
        )
        .leftMap[ValidationError](_ => ValidationError.OverflowError)

    def checkNegativeAmount: Either[ValidationError, Unit] =
      Either.cond(transfers.forall(_.amount >= 0), (), ValidationError.GenericError("One of the transfers has negative amount"))

    def checkMaxCount: Either[ValidationError, Unit] = Either.cond(
      !(transfers.lengthCompare(MaxTransferCount) > 0),
      (),
      ValidationError.GenericError(s"Number of transfers ${transfers.length} is greater than $MaxTransferCount")
    )

    checkMaxCount >> checkOverflow >> checkNegativeAmount
  }

  def validateName(name: Array[Byte]): Either[ValidationError, Array[Byte]] = {
    Either
      .cond(
        name.length >= MinAssetNameLength && name.length <= MaxAssetNameLength,
        name,
        ValidationError.InvalidName
      )
  }

  def validateDescription(description: Array[Byte]): Either[ValidationError, Array[Byte]] = {
    Either
      .cond(
        description.length <= MaxDescriptionLength,
        description,
        ValidationError.TooBigArray
      )
  }

  def validateAttachment(attachment: Array[Byte]): Either[ValidationError, Array[Byte]] = {
    Either
      .cond(
        attachment.length <= TransferValidation.MaxAttachmentSize,
        attachment,
        ValidationError.TooBigArray
      )
  }

  def validateDecimals(decimals: Byte): Either[ValidationError, Byte] = {
    Either
      .cond(
        decimals >= 0 && decimals <= MaxDecimals,
        decimals,
        ValidationError.TooBigArray
      )
  }

  def validateChainId(chainId: Byte): Either[GenericError, Unit] = {
    val currentChainId = AddressScheme.getAddressSchema.chainId
    Either.cond(
      chainId == currentChainId,
      (),
      GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId")
    )
  }
}
