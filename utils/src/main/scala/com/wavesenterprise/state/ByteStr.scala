package com.wavesenterprise.state

import com.wavesenterprise.utils.{Base58, Base64}
import play.api.libs.json._
import pureconfig.ConfigReader

import scala.util.Try

case class ByteStr(arr: Array[Byte]) {
  override def equals(a: Any): Boolean = a match {
    case other: ByteStr => arr.sameElements(other.arr)
    case _              => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)

  lazy val base58: String = Base58.encode(arr)

  lazy val base64: String = "base64:" + Base64.encode(arr)

  lazy val trim: String = base58.take(7) + "..."

  override def toString: String = base58
}

object ByteStr {
  def decodeBase58(s: String): Try[ByteStr] = Base58.decode(s).map(ByteStr(_))
  def decodeBase64(s: String): Try[ByteStr] = Base64.decode(s).map(ByteStr(_))
  val empty: ByteStr                        = ByteStr(Array.emptyByteArray)

  implicit val ByteStrFormat: Format[ByteStr] = new Format[ByteStr] {
    override def writes(o: ByteStr): JsValue = JsString(o.base58)
    override def reads(json: JsValue): JsResult[ByteStr] = json match {
      case JsString(v) => decodeBase58(v).fold(e => JsError(s"Error parsing base58: ${e.getMessage}"), b => JsSuccess(b))
      case _           => JsError("Expected JsString")
    }
  }

  implicit val configReader: ConfigReader[ByteStr] = ConfigReader.fromStringTry[ByteStr](decodeBase58)
}
