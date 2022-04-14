package com.wavesenterprise.privacy

import cats.syntax.either._
import com.wavesenterprise.privacy.PolicyDataHash.PolicyDataHashImpl
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.{GenericError, InvalidPolicyDataHash}
import com.wavesenterprise.utils.Base64
import play.api.libs.json.{JsString, Writes}
import scorex.crypto.hash.Sha256

sealed trait PolicyDataHash {
  def bytes: ByteStr

  lazy val stringRepr: String   = bytes.base58
  override def toString: String = stringRepr

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: PolicyDataHashImpl => bytes == a.bytes
    case _                     => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object PolicyDataHash {

  val DataHashLength: Int = 32

  private case class PolicyDataHashImpl(bytes: ByteStr) extends PolicyDataHash

  def fromBase58String(str: String): Either[ValidationError, PolicyDataHash] = {
    ByteStr
      .decodeBase58(str)
      .toEither
      .ensure(GenericError("Invalid policy data hash length"))(_.arr.length == DataHashLength)
      .map(PolicyDataHashImpl)
      .leftMap(e => GenericError(e.toString))
  }

  def deserialize(bytes: Array[Byte]): PolicyDataHash = {
    PolicyDataHashImpl(ByteStr(bytes))
  }

  def fromDataBytes(publicData: Array[Byte]): PolicyDataHash = {
    val bytes = Sha256.hash(publicData)
    PolicyDataHashImpl(ByteStr(bytes))
  }

  def fromDataString(base64Str: String): Either[ValidationError, PolicyDataHash] = {
    Base64
      .decode(base64Str)
      .toEither
      .leftMap(ex => InvalidPolicyDataHash(s"Unable to decode base64: ${ex.getMessage}"))
      .map(fromDataBytes)
  }

  implicit val writes: Writes[PolicyDataHash] = h => JsString(h.stringRepr)

}
