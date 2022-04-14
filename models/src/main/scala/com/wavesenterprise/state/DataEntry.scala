package com.wavesenterprise.state

import cats.Show
import play.api.libs.json._
import pureconfig.ConfigReader

sealed abstract class DataEntry[T](val `type`: String, val key: String, val value: T) {

  def toJson: JsObject = Json.obj("key" -> key, "type" -> `type`)
}

object DataEntry {

  object Type extends Enumeration {
    val Integer: Type.Value = Value(0)
    val Boolean: Type.Value = Value(1)
    val Binary: Type.Value  = Value(2)
    val String: Type.Value  = Value(3)
  }

  implicit val ordering: Ordering[DataEntry[_]] = Ordering.by(_.key)

  implicit object Format extends Format[DataEntry[_]] {
    def reads(jsv: JsValue): JsResult[DataEntry[_]] = {
      jsv \ "key" match {
        case JsDefined(JsString(key)) =>
          jsv \ "type" match {
            case JsDefined(JsString("integer")) =>
              jsv \ "value" match {
                case JsDefined(JsNumber(n)) =>
                  try {
                    JsSuccess(IntegerDataEntry(key, n.toLongExact))
                  } catch {
                    case _: ArithmeticException =>
                      JsError(s"value $n on key $key is too big for integer (max value = ${Long.MaxValue}) or is not an integer")
                  }
                case _ => JsError("value is missing or is not an integer")
              }
            case JsDefined(JsString("boolean")) =>
              jsv \ "value" match {
                case JsDefined(JsBoolean(b)) => JsSuccess(BooleanDataEntry(key, b))
                case _                       => JsError("value is missing or is not a boolean value")
              }
            case JsDefined(JsString("binary")) =>
              jsv \ "value" match {
                case JsDefined(JsString(enc)) =>
                  ByteStr.decodeBase64(enc).fold(ex => JsError(ex.getMessage), bstr => JsSuccess(BinaryDataEntry(key, bstr)))
                case _ => JsError("value is missing or is not a string")
              }
            case JsDefined(JsString("string")) =>
              jsv \ "value" match {
                case JsDefined(JsString(str)) => JsSuccess(StringDataEntry(key, str))
                case _                        => JsError("value is missing or is not a string")
              }
            case JsDefined(JsString(t)) => JsError(s"unknown type $t")
            case _                      => JsError("type is missing")
          }
        case _ => JsError("key is missing")
      }
    }

    def writes(item: DataEntry[_]): JsValue = item.toJson
  }

  implicit val configReader: ConfigReader[DataEntry[_]] = {
    ConfigReader.forProduct3[DataEntry[_], String, String, String]("key", "type", "value") {
      case (k, t, v) =>
        t.toLowerCase match {
          case "integer" => IntegerDataEntry(k, v.toLong)
          case "boolean" => BooleanDataEntry(k, v.toBoolean)
          case "string"  => StringDataEntry(k, v)
          case "binary" =>
            ByteStr
              .decodeBase64(v)
              .fold(ex => throw new RuntimeException(s"Couldn't decode DataEntry's binary value for key '$k'", ex), BinaryDataEntry(k, _))
          case undefined => throw new RuntimeException(s"Unknown DataEntry type '$undefined' for key '$k'")
        }
    }
  }

  implicit val toPrintable: Show[DataEntry[_]] = { x =>
    import x._
    s"""
       |key: $key
       |type: ${`type`}
       |value: ${value.toString}""".stripMargin
  }
}

case class IntegerDataEntry(override val key: String, override val value: Long) extends DataEntry[Long]("integer", key, value) {

  override def toJson: JsObject = super.toJson + ("value" -> JsNumber(value))
}

case class BooleanDataEntry(override val key: String, override val value: Boolean) extends DataEntry[Boolean]("boolean", key, value) {

  override def toJson: JsObject = super.toJson + ("value" -> JsBoolean(value))
}

case class BinaryDataEntry(override val key: String, override val value: ByteStr) extends DataEntry[ByteStr]("binary", key, value) {

  override def toJson: JsObject = super.toJson + ("value" -> JsString(value.base64))
}

case class StringDataEntry(override val key: String, override val value: String) extends DataEntry[String]("string", key, value) {

  override def toJson: JsObject = super.toJson + ("value" -> JsString(value))
}
