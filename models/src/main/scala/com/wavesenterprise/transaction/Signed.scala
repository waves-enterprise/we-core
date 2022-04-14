package com.wavesenterprise.transaction

import com.wavesenterprise.transaction.ValidationError.InvalidSignature
import monix.eval.Coeval

trait Signed extends Authorized {
  val signatureValid: Coeval[Boolean]
}

object Signed {

  def validate[S <: Signed](s: S): Either[InvalidSignature, S] = {
    Either.cond(s.signatureValid(), s, InvalidSignature(s, None))
  }
}
