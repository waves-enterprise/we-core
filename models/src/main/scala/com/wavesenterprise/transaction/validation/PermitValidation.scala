package com.wavesenterprise.transaction.validation

import com.wavesenterprise.acl.PermissionOp
import com.wavesenterprise.transaction.ValidationError

object PermitValidation {

  def validatePermissionOp(txTimestamp: Long, permissionOp: PermissionOp): Either[ValidationError, Unit] =
    Either.cond(
      permissionOp.timestamp == txTimestamp,
      (),
      ValidationError.GenericError("Invalid PermitTransaction: timestamp for tx must match timestamp from PermissionOp")
    )
}
