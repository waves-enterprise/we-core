package com.wavesenterprise.crypto.internals.confidentialcontracts

import com.wavesenterprise.crypto
import com.wavesenterprise.crypto.internals._
import com.wavesenterprise.state.ByteStr
import play.api.libs.json.{JsError, JsString, JsSuccess, Reads, Writes}

case class Commitment(hash: ByteStr) extends AnyVal {

  def validate(data: Array[Byte], salt: SaltBytes): Boolean = {
    val actualHash = com.wavesenterprise.crypto.algorithms.saltedSecureHash(data, salt)

    hash == actualHash.bytes
  }

  /**
    * Use only for debug/test purposes
    */
  def validateWithCustomAlgorithms(data: Array[Byte], salt: SaltBytes, cryptoAlgorithms: CryptoAlgorithms[_]): Boolean = {
    val actualHash = cryptoAlgorithms.saltedSecureHash(data, salt)

    hash == actualHash.bytes
  }

}

object Commitment {
  def create(data: Array[Byte], salt: SaltBytes): Commitment = {
    val hash = crypto.algorithms.saltedSecureHash(data, salt)

    Commitment(ByteStr(hash.arr))
  }

  /**
    * Use only for debug/test purposes
    */
  def createFromCustomAlgorithms(data: Array[Byte], salt: SaltBytes, cryptoAlgorithms: CryptoAlgorithms[_]): Commitment = {
    val hash = cryptoAlgorithms.saltedSecureHash(data, salt)

    Commitment(ByteStr(hash.arr))
  }

  def fromHash(hash: HashBytes): Commitment = {
    Commitment(ByteStr(hash.arr))
  }

  def fromBase58(base58: String): Either[CryptoError, Commitment] = {
    ByteStr.decodeBase58(base58).toEither
      .left.map(ex => InvalidHash(s"Unable to decode base58: ${ex.getMessage}"))
      .map(Commitment(_))
  }

  val commitmentLength: Int = crypto.DigestSize

  implicit val writes: Writes[Commitment] = commitment => JsString(commitment.hash.toString)

  implicit val reads: Reads[Commitment] = {
    case JsString(value) => fromBase58(value).fold(error => JsError(error.message), JsSuccess(_))
    case _               => JsError("Expected string value")
  }

}
