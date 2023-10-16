package com.wavesenterprise.crypto.internals

import pureconfig.error.FailureReason

sealed trait CryptoError {
  def message: String
}

case class InvalidAddress(message: String)                    extends CryptoError
case class InvalidHash(message: String)                       extends CryptoError
case class InvalidPublicKey(message: String)                  extends CryptoError
case class GenericError(message: String)                      extends CryptoError
case class DecryptionError(message: String, cause: Throwable) extends CryptoError
case class PkiError(message: String)                          extends CryptoError

trait PkiConfigError extends FailureReason

case class UnknownValueError[T](stringValue: String, expectedValues: Seq[T]) extends PkiConfigError {
  override def description: String = s"Value '$stringValue' is unknown. Expected values are: [${expectedValues.mkString(", ")}]"
}
