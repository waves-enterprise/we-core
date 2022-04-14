package com.wavesenterprise.transaction.validation

import com.wavesenterprise.state.DataEntry
import com.wavesenterprise.transaction.{DataTransaction, DataTransactionEntryOps, ValidationError}

object DataValidation {

  val MaxBytes: Int      = 150 * 1024
  val MaxEntryCount: Int = 100

  def validateData(data: List[DataEntry[_]]): Either[ValidationError, Unit] = {
    if (data.lengthCompare(MaxEntryCount) > 0 || data.exists(DataTransactionEntryOps.validate(_).isLeft)) {
      Left(ValidationError.TooBigArray)
    } else if (data.exists(_.key.isEmpty)) {
      Left(ValidationError.GenericError("Empty key found"))
    } else if (data.map(_.key).distinct.lengthCompare(data.size) < 0) {
      Left(ValidationError.GenericError("Duplicate keys found"))
    } else {
      Right(())
    }
  }

  def validateSize(tx: DataTransaction): Either[ValidationError, DataTransaction] = {
    Either.cond(tx.bytes().length <= MaxBytes, tx, ValidationError.TooBigArray)
  }
}
