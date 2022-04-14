package com.wavesenterprise.crypto.internals

import scorex.util.encode.Base58

trait PrivateKey extends Product with Serializable

case class WavesPrivateKey(private[crypto] val internal: Array[Byte]) extends PrivateKey {
  override def hashCode(): Int = {
    java.util.Arrays.hashCode(internal)
  }

  def base58: String =
    Base58.encode(internal)

  def canEqual(other: Any): Boolean = other.isInstanceOf[WavesPrivateKey]

  override def equals(other: Any): Boolean = other match {
    case that: WavesPrivateKey =>
      (that canEqual this) &&
        internal.sameElements(that.internal)
    case _ => false
  }
}
