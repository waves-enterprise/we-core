package com.wavesenterprise.docker

import com.google.common.io.ByteStreams.newDataOutput
import com.google.common.primitives.Shorts
import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.InvalidContractApiVersion
import com.wavesenterprise.utils.EitherUtils.EitherExt
import play.api.libs.json.{Format, JsError, JsString, JsSuccess}
import wavesenterprise.{ApiVersionProto, CurrentContractApiVersion}

import scala.util.matching.Regex

case class ContractApiVersion(majorVersion: Short, minorVersion: Short) {

  require(majorVersion >= 0 && minorVersion >= 0, "ContractApiVersion values must be positive")

  override def toString: String = s"$majorVersion.$minorVersion"

  def bytes: Array[Byte] = {
    //noinspection UnstableApiUsage
    val output = newDataOutput(Shorts.BYTES * 2)
    output.writeShort(majorVersion)
    output.writeShort(minorVersion)
    output.toByteArray
  }
}

object ContractApiVersion extends {
  private val StringPattern: Regex = """^(\d+)\.(\d+).*""".r

  val `1.0`: ContractApiVersion   = ContractApiVersion(1, 0)
  val Initial: ContractApiVersion = `1.0`
  val Current: ContractApiVersion = fromString {
    CurrentContractApiVersion.scalaDescriptor.getOptions.extension(ApiVersionProto.contractApiVersion)
  }.explicitGet()

  val values: Vector[ContractApiVersion] = Vector(`1.0`, Current)

  def fromBytesUnsafe(bytes: Array[Byte], position: Int): (ContractApiVersion, Int) = {
    val majorVersion = Shorts.fromBytes(bytes(position), bytes(position + 1))
    val minorVersion = Shorts.fromBytes(bytes(position + 2), bytes(position + 3))
    ContractApiVersion(majorVersion, minorVersion) -> (position + Shorts.BYTES * 2)
  }

  def fromString(str: String): Either[ValidationError, ContractApiVersion] = {
    str match {
      case StringPattern(major, minor) =>
        Either.cond(
          BigInt(major).isValidShort && BigInt(minor).isValidShort,
          ContractApiVersion(major.toShort, minor.toShort),
          InvalidContractApiVersion(s"Contract API version values must be within the [0-${Short.MaxValue}] range")
        )
      case _ => Left(InvalidContractApiVersion(s"Invalid contract api version. Expected string pattern '$StringPattern'"))
    }
  }

  implicit val format: Format[ContractApiVersion] = {
    Format(
      {
        case JsString(StringPattern(majorVersion, minorVersion)) =>
          JsSuccess(ContractApiVersion(majorVersion.toShort, minorVersion.toShort))
        case _ =>
          JsError(s"Invalid contract api version. Expected string pattern '$StringPattern'")
      }, { version =>
        JsString(version.toString)
      }
    )
  }
}
