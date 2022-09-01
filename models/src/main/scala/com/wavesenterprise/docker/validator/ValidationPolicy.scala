package com.wavesenterprise.docker.validator

import com.google.common.io.ByteStreams.newDataOutput
import com.wavesenterprise.account.Address
import com.wavesenterprise.serialization.ModelsBinarySerializer
import enumeratum.values.{ByteEnum, ByteEnumEntry}
import play.api.libs.json._

import scala.collection.immutable

sealed abstract class ValidationPolicyDescriptor(val value: Byte, val name: String) extends ByteEnumEntry

object ValidationPolicyDescriptor extends ByteEnum[ValidationPolicyDescriptor] {
  case object Any               extends ValidationPolicyDescriptor(0, "any")
  case object Majority          extends ValidationPolicyDescriptor(1, "majority")
  case object MajorityWithOneOf extends ValidationPolicyDescriptor(2, "majority_with_one_of")

  override val values: immutable.IndexedSeq[ValidationPolicyDescriptor] = findValues
}

sealed trait ValidationPolicy {
  def name: String
  def bytes: Array[Byte]
}

object ValidationPolicy {

  val Default: ValidationPolicy = Any
  val MajorityRatio: Double     = 2.0 / 3

  case object Any extends ValidationPolicy {
    override def name: String       = ValidationPolicyDescriptor.Any.name
    override def bytes: Array[Byte] = Array(ValidationPolicyDescriptor.Any.value)
  }

  case object Majority extends ValidationPolicy {
    override def name: String       = ValidationPolicyDescriptor.Majority.name
    override def bytes: Array[Byte] = Array(ValidationPolicyDescriptor.Majority.value)
  }

  case class MajorityWithOneOf(addresses: List[Address]) extends ValidationPolicy {
    override def name: String = ValidationPolicyDescriptor.MajorityWithOneOf.name
    override def bytes: Array[Byte] = {
      //noinspection UnstableApiUsage
      val output = newDataOutput()
      output.writeByte(ValidationPolicyDescriptor.MajorityWithOneOf.value)
      ModelsBinarySerializer.writeAddresses(addresses, output)
      output.toByteArray
    }
  }

  def fromBytesUnsafe(bytes: Array[Byte], position: Int): (ValidationPolicy, Int) =
    bytes(position) match {
      case ValidationPolicyDescriptor.Any.value =>
        Any -> (position + 1)
      case ValidationPolicyDescriptor.Majority.value =>
        Majority -> (position + 1)
      case ValidationPolicyDescriptor.MajorityWithOneOf.value =>
        val (addresses, end) = ModelsBinarySerializer.parseAddresses(bytes, position + 1)
        MajorityWithOneOf(addresses) -> end
    }

  implicit val format: Format[ValidationPolicy] = {
    val typeFieldName      = "type"
    val addressesFieldName = "addresses"

    Format(
      {
        case JsObject(fields) if fields.get(typeFieldName).contains(JsString(ValidationPolicyDescriptor.Any.name)) =>
          JsSuccess(ValidationPolicy.Any)
        case JsObject(fields) if fields.get(typeFieldName).contains(JsString(ValidationPolicyDescriptor.Majority.name)) =>
          JsSuccess(ValidationPolicy.Majority)
        case JsObject(fields) if fields.get(typeFieldName).contains(JsString(ValidationPolicyDescriptor.MajorityWithOneOf.name)) =>
          fields
            .get(addressesFieldName)
            .map(Reads.list[Address].reads)
            .getOrElse(JsError(s"Field '$addressesFieldName' not found"))
            .map(addresses => ValidationPolicy.MajorityWithOneOf(addresses))
        case _ =>
          JsError(s"Expected validation policy json object")
      }, {
        case policy @ (Any | Majority) =>
          Json.obj(typeFieldName -> policy.name)
        case policy @ MajorityWithOneOf(addresses) =>
          Json.obj(
            typeFieldName      -> policy.name,
            addressesFieldName -> Json.toJson(addresses)
          )
      }
    )
  }
}
