package com.wavesenterprise.lang.v1.evaluator.ctx.impl

import com.wavesenterprise.lang.v1.compiler.Terms._
import scodec.bits.ByteVector

package object converters {

  implicit def c(b: ByteVector): EVALUATED             = CONST_BYTEVECTOR(b)
  implicit def c(b: String): EVALUATED                 = CONST_STRING(b)
  implicit def c(b: Long): EVALUATED                   = CONST_LONG(b)
  implicit def c(b: Boolean): EVALUATED                = CONST_BOOLEAN(b)
  implicit def c(is: IndexedSeq[EVALUATED]): EVALUATED = ARR(is)

  implicit def fromOptionBV[T](v: Option[ByteVector]): EVALUATED = v.map(CONST_BYTEVECTOR).getOrElse(unit)
  implicit def fromOptionL[T](v: Option[Long]): EVALUATED        = v.map(CONST_LONG).getOrElse(unit)
  implicit def fromOptionS[T](v: Option[String]): EVALUATED      = v.map(CONST_STRING).getOrElse(unit)
  implicit def fromOptionB[T](v: Option[Boolean]): EVALUATED     = v.map(CONST_BOOLEAN).getOrElse(unit)
  implicit def fromOptionCO[T](v: Option[CaseObj]): EVALUATED    = v.getOrElse(unit)

}
