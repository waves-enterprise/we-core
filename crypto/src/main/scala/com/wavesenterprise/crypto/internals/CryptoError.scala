package com.wavesenterprise.crypto.internals

sealed trait CryptoError {
  def message: String
}

case class InvalidAddress(message: String)                    extends CryptoError
case class InvalidPublicKey(message: String)                  extends CryptoError
case class GenericError(message: String)                      extends CryptoError
case class DecryptionError(message: String, cause: Throwable) extends CryptoError
