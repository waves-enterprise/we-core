package com.wavesenterprise.serialization

import monix.eval.Coeval

trait BytesSerializable {
  val bytes: Coeval[Array[Byte]]
}
