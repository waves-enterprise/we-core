package com.wavesenterprise.crypto.internals

import java.io.File

/**
  * Wrapper over CryptoAlgorithms with keyStore for type safety
  */
abstract class CryptoContext {
  type KeyPair0 <: KeyPair
  type PublicKey0  = KeyPair0#PublicKey0
  type PrivateKey0 = KeyPair0#PrivateKey0

  def algorithms: CryptoAlgorithms[KeyPair0]

  def keyStore(file: Option[File], password: Array[Char]): KeyStore[KeyPair0]
}
