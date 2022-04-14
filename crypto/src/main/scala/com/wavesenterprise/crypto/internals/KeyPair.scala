package com.wavesenterprise.crypto.internals

trait KeyPair extends Product with Serializable {
  type PublicKey0 <: PublicKey
  type PrivateKey0 <: PrivateKey

  def getPublic: PublicKey0
  def getPrivate: PrivateKey0
}

case class WavesKeyPair(getPrivate: WavesPrivateKey, getPublic: WavesPublicKey) extends KeyPair {
  override type PublicKey0  = WavesPublicKey
  override type PrivateKey0 = WavesPrivateKey
}
