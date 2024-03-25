package com.wavesenterprise.transaction.docker

import com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment
import com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment.commitmentLength
import com.wavesenterprise.transaction.ValidationError

trait ConfidentialDataInCallContractSupported {
  def inputCommitment: Commitment
}

trait OptionalConfidentialDataInCallContractSupported {
  def inputCommitmentOpt: Option[Commitment]
}

object CommitmentValidations {

  def validateCommitment(commitment: Commitment): Either[ValidationError, Unit] =
    Either.cond(
      commitment.hash.arr.length == commitmentLength,
      (),
      ValidationError.GenericError(
        s"Invalid commitment hash length: '${commitment.hash.arr.length}', it should be current crypto digest size: '$commitmentLength'")
    )
}
