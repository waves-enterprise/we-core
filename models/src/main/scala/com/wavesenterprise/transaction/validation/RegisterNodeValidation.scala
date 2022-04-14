package com.wavesenterprise.transaction.validation

import com.wavesenterprise.acl.OpType
import com.wavesenterprise.transaction.ValidationError

object RegisterNodeValidation {
  val NodeNameMaxLength: Int = 100 //in symbols

  def validateNodeName(maybeName: Option[String]): Either[ValidationError, Unit] =
    Either.cond(
      maybeName.forall(_.length < NodeNameMaxLength),
      (),
      ValidationError.GenericError(s"NodeName length should be less than $NodeNameMaxLength")
    )

  def validateOpType(opType: OpType, maybeNodeName: Option[String]): Either[ValidationError, Unit] =
    Either.cond(
      opType == OpType.Remove || maybeNodeName.exists(_.nonEmpty),
      (),
      ValidationError.GenericError(s"NodeName should not be empty")
    )
}
