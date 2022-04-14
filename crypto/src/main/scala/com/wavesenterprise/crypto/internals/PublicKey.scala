package com.wavesenterprise.crypto.internals

trait PublicKey extends Product with Serializable {
  def getEncoded: Array[Byte]
  def length: Int = getEncoded.length
}

case class WavesPublicKey(getEncoded: Array[Byte]) extends PublicKey {
  override def hashCode(): Int = java.util.Arrays.hashCode(getEncoded)

  def canEqual(other: Any): Boolean = other.isInstanceOf[WavesPublicKey]

  override def equals(other: Any): Boolean = other match {
    case that: WavesPublicKey =>
      (that canEqual this) &&
        getEncoded.sameElements(that.getEncoded)
    case _ => false
  }
}
