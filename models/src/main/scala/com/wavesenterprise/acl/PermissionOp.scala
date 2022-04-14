package com.wavesenterprise.acl

import com.google.common.primitives.{Bytes, Longs}
import com.wavesenterprise.serialization.JsonSerializable
import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.utils.EitherUtils.EitherExt
import monix.eval.Coeval
import play.api.libs.json.{JsObject, JsString, Json, Writes}

import scala.util.Try

case class PermissionOp(opType: OpType, role: Role, timestamp: Long, dueTimestampOpt: Option[Long]) extends JsonSerializable {

  val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "opType"       -> opType.str,
      "role"         -> role.prefixS,
      "dueTimestamp" -> dueTimestampOpt
    )
  }

  def isActive(forTimestamp: Long): Boolean =
    opType == OpType.Add && dueTimestampOpt.forall(_ > forTimestamp)

  def isActiveWithoutDueDate: Boolean =
    opType == OpType.Add && dueTimestampOpt.isEmpty

  def bytes: Array[Byte] =
    Bytes.concat(
      Array(opType.byte, role.byte),
      Longs.toByteArray(timestamp),
      dueTimestampOpt
        .fold(Array(0: Byte) ++ Array.fill(Longs.BYTES)(0: Byte))(ts => Array(1: Byte) ++ Longs.toByteArray(ts))
    )
}

object PermissionOp {
  def fromBytes(bytes: Array[Byte]): Either[ValidationError, PermissionOp] = {
    for {
      opByte <- bytes.headOption.toRight(ValidationError.GenericError("Missing operation type"))
      opType <- OpType.fromByte(opByte)

      withoutOp = bytes.tail
      roleByte <- withoutOp.headOption.toRight(ValidationError.GenericError("Missing role type"))
      role     <- Role.fromByte(roleByte)

      timestamp <- Try(Longs.fromByteArray(withoutOp.tail)).toEither.left.map(_ => ValidationError.GenericError("Missing timestamp"))

      dueTsBytes = withoutOp.tail.drop(Longs.BYTES)

      dueTimestampDefined <- Right(dueTsBytes.headOption)
      dueTimestampOpt <- dueTimestampDefined match {
        case Some(headByte) =>
          if (headByte == 1)
            Try(Longs.fromByteArray(dueTsBytes.tail)).fold(err => Left(GenericError(err)), ts => Right(Some(ts)))
          else
            Right(None)
        case _ => Left(ValidationError.GenericError("Missing dueTimestampOpt field"))
      }
    } yield PermissionOp(opType, role, timestamp, dueTimestampOpt)
  }

  def fromBytesUnsafe(bytes: Array[Byte]): PermissionOp = fromBytes(bytes).explicitGet()

  def addUnlimited(role: Role, timestamp: Long): PermissionOp =
    PermissionOp(OpType.Add, role, timestamp, None)

  val serializedSize: Int = 2 + Longs.BYTES + (1 + Longs.BYTES)

  implicit val writes: Writes[PermissionOp] = op => op.json()
}

sealed trait OpType {
  def byte: Byte
  def str: String
  def opposite: OpType
}

object OpType {
  def fromByte(byte: Byte): Either[GenericError, OpType] = {
    byte match {
      case Add.byte    => Right(Add)
      case Remove.byte => Right(Remove)
      case unknownByte => Left(ValidationError.GenericError(s"OpType.fromByte failure: unknown operation byte $unknownByte"))
    }
  }

  def fromByteUnsafe(byte: Byte): OpType = fromByte(byte).explicitGet()

  def fromStr(str: String): Either[GenericError, OpType] = str.toLowerCase match {
    case "add"    => Right(OpType.Add)
    case "remove" => Right(OpType.Remove)
    case other    => Left(ValidationError.GenericError(s"OpType.fromStr failure: unknown operation string $other"))
  }

  case object Add extends OpType {
    val byte     = 'a'
    val str      = "add"
    val opposite = Remove
  }

  case object Remove extends OpType {
    val byte     = 'r'
    val str      = "remove"
    val opposite = Add
  }

  implicit val writes: Writes[OpType] = opType => JsString(opType.str)
}
