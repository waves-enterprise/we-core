package com.wavesenterprise.lang.v1.traits

import com.wavesenterprise.lang.v1.compiler.Types._

sealed abstract case class DataType(innerType: REAL)
object DataType {
  object Boolean   extends DataType(BOOLEAN)
  object Long      extends DataType(LONG)
  object ByteArray extends DataType(BYTEVECTOR)
  object String    extends DataType(STRING)
}
