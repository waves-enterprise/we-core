package com.wavesenterprise.transaction.validation

import com.wavesenterprise.account.AddressScheme
import com.wavesenterprise.crypto
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.GenericError

import scala.util.{Either, Try}

object LeaseValidation {
  def validateLeaseParams(amount: Long, fee: Long): Either[ValidationError, Unit] =
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount(amount, "WEST"))
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError)
    } else {
      Right(())
    }

  def validateLeaseId(leaseId: ByteStr): Either[ValidationError.GenericError, Unit] = {
    if (leaseId.arr.length != crypto.DigestSize) {
      Left(ValidationError.GenericError("Lease transaction id is invalid"))
    } else {
      Right(())
    }
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
