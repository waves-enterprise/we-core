package com.wavesenterprise.transaction.docker

import com.google.common.io.ByteArrayDataOutput
import com.google.common.io.ByteStreams.newDataOutput
import com.wavesenterprise.crypto
import com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment
import com.wavesenterprise.serialization.BinarySerializer
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.GenericError

import com.wavesenterprise.transaction.docker.ReadDescriptorType.{
  FilteredKeysReadDescriptorType,
  SpecificBalancesReadDescriptorType,
  SpecificKeysReadDescriptorType
}
import com.wavesenterprise.transaction.docker.ReadingsHash.readingsHashLength
import enumeratum.values.{ByteEnum, ByteEnumEntry}
import play.api.libs.json.{Format, JsError, JsObject, JsString, JsSuccess, Json, Reads, Writes}

import scala.collection.immutable

trait ConfidentialCallOutputSupport {
  def readings: List[ReadDescriptor]

  def readingsHash: Option[ReadingsHash]

  def outputCommitment: Option[Commitment]
}

object ConfidentialCallOutputSupport {
  def validateReadingsHash(readingsHashOpt: Option[ReadingsHash]): Either[ValidationError, Unit] =
    readingsHashOpt match {
      case Some(readingsHash) if readingsHash.hash.arr.length != readingsHashLength =>
        Left(ValidationError.GenericError(
          s"Invalid readings hash length: '${readingsHash.hash.arr.length}', it should be current crypto digest size: $readingsHashLength"))
      case _ => Right(())
    }
}

case class ReadingsHash(hash: ByteStr) extends AnyVal {
  def isEmpty: Boolean = hash == ByteStr.empty
}

object ReadingsHash {
  val readingsHashLength: Int = crypto.DigestSize

  def fromReadings(readings: Seq[ReadDescriptor]): ReadingsHash = {
    val readingsByteArray = {
      val output = newDataOutput()
      readings.foreach(readDescriptor => ReadDescriptor.writeBytes(readDescriptor, output))
      output.toByteArray
    }
    val readingsHash: Array[Byte] = crypto.algorithms.secureHash(readingsByteArray)
    ReadingsHash(ByteStr(readingsHash))
  }

  implicit val writes: Writes[ReadingsHash] = readingsHash => JsString(readingsHash.hash.toString)

  implicit val reads: Reads[ReadingsHash] = {
    case JsString(value) => ByteStr.decodeBase58(value).toEither.fold(error => JsError(error.toString), success => JsSuccess(ReadingsHash(success)))
    case _               => JsError("Expected string value")
  }
}

sealed abstract class ReadDescriptorType(val value: Byte, val name: String) extends ByteEnumEntry

object ReadDescriptorType extends ByteEnum[ReadDescriptorType] {

  case object SpecificKeysReadDescriptorType extends ReadDescriptorType(0, "specific-keys")

  case object FilteredKeysReadDescriptorType extends ReadDescriptorType(1, "filtered-keys")

  case object SpecificBalancesReadDescriptorType extends ReadDescriptorType(2, "specific-balances")

  override def values: immutable.IndexedSeq[ReadDescriptorType] = findValues

  def fromString(str: String): Either[GenericError, ReadDescriptorType] = {
    values
      .find(_.name == str)
      .toRight(GenericError(s"ReadDescriptorType type '$str' does not exist"))
  }

  implicit val readDescriptorTypeFormat: Format[ReadDescriptorType] = Format(
    {
      case JsString(str) =>
        ReadDescriptorType
          .fromString(str)
          .fold(
            error => JsError(s"Error parsing 'readDescriptorType' field: ${error.err}"),
            parsed => JsSuccess(parsed)
          )
      case otherType =>
        JsError(s"Error parsing 'readDescriptorType' field: expected a string, but got '$otherType'")
    },
    { case readDescriptorType: ReadDescriptorType => JsString(readDescriptorType.name) }
  )

}

sealed abstract class ReadDescriptor(val readDescriptorType: ReadDescriptorType)

object ReadDescriptor {

  def writeBytes(readDescriptor: ReadDescriptor, output: ByteArrayDataOutput): Unit = {
    output.writeByte(readDescriptor.readDescriptorType.value)
    readDescriptor match {
      case specificKeys: SpecificKeys         => SpecificKeys.writeBytes(specificKeys, output)
      case filteredKeys: FilteredKeys         => FilteredKeys.writeBytes(filteredKeys, output)
      case specificBalances: SpecificBalances => SpecificBalances.writeBytes(specificBalances, output)
    }
  }

  def fromBytes(bytes: Array[Byte], offset: Int): (ReadDescriptor, Int) = {
    val (t, typeEnd) = ReadDescriptorType.valuesToEntriesMap(bytes(offset)) -> (offset + 1)
    t match {
      case SpecificKeysReadDescriptorType     => SpecificKeys.fromBytes(bytes, typeEnd)
      case FilteredKeysReadDescriptorType     => FilteredKeys.fromBytes(bytes, typeEnd)
      case SpecificBalancesReadDescriptorType => SpecificBalances.fromBytes(bytes, typeEnd)
    }
  }

  implicit val format: Format[ReadDescriptor] = Format(
    {
      case obj @ JsObject(fields) =>
        fields.get("keys") match {
          case None    => FilteredKeys.format.reads(obj)
          case Some(_) => SpecificKeys.format.reads(obj)
        }
      case _ => JsError(s"Expected ReadDescriptor json object")
    },
    {
      case obj: SpecificKeys => SpecificKeys.format.writes(obj)
      case obj: FilteredKeys => FilteredKeys.format.writes(obj)
    }
  )
}

case class SpecificKeys(contractId: ByteStr, keys: Seq[String]) extends ReadDescriptor(ReadDescriptorType.SpecificKeysReadDescriptorType)

object SpecificKeys {

  def writeBytes(value: SpecificKeys, output: ByteArrayDataOutput): Unit = {
    BinarySerializer.writeShortByteStr(value.contractId, output)
    BinarySerializer.writeShortIterable(value.keys, BinarySerializer.writeShortString, output)
  }

  def fromBytes(bytes: Array[Byte], offset: Int): (SpecificKeys, Int) = {
    val (contractId, contractIdEnd) = BinarySerializer.parseShortByteStr(bytes, offset)
    val (keys, end)                 = BinarySerializer.parseShortList(bytes, BinarySerializer.parseShortString, contractIdEnd)

    SpecificKeys(contractId, keys) -> end
  }

  implicit val format: Format[SpecificKeys] = Json.format
}

case class FilteredKeys(contractId: ByteStr, matches: Option[String], offset: Option[Int], limit: Option[Int])
    extends ReadDescriptor(ReadDescriptorType.FilteredKeysReadDescriptorType)

object FilteredKeys {

  def writeBytes(value: FilteredKeys, output: ByteArrayDataOutput): Unit = {
    BinarySerializer.writeShortByteStr(value.contractId, output)
    BinarySerializer.writeByteIterable(value.matches, BinarySerializer.writeShortString, output)
    BinarySerializer.writeByteIterable(value.offset, BinarySerializer.writeInt, output)
    BinarySerializer.writeByteIterable(value.limit, BinarySerializer.writeInt, output)
  }

  def fromBytes(bytes: Array[Byte], offset: Int): (FilteredKeys, Int) = {
    val (contractId, contractIdEnd)                 = BinarySerializer.parseShortByteStr(bytes, offset)
    val (matches, matchesEnd)                       = BinarySerializer.parseOption(bytes, BinarySerializer.parseShortString, contractIdEnd)
    val (filteredKeysOffset, filteredKeysOffsetEnd) = BinarySerializer.parseOption(bytes, BinarySerializer.parseInt, matchesEnd)
    val (limit, end)                                = BinarySerializer.parseOption(bytes, BinarySerializer.parseInt, filteredKeysOffsetEnd)

    FilteredKeys(contractId, matches, filteredKeysOffset, limit) -> end
  }

  implicit val format: Format[FilteredKeys] = Json.format
}

case class SpecificBalances(contractId: ByteStr, assetIds: Seq[String]) extends ReadDescriptor(ReadDescriptorType.SpecificBalancesReadDescriptorType)

object SpecificBalances {

  def writeBytes(value: SpecificBalances, output: ByteArrayDataOutput): Unit = {
    BinarySerializer.writeShortByteStr(value.contractId, output)
    BinarySerializer.writeShortIterable(value.assetIds, BinarySerializer.writeShortString, output)
  }

  def fromBytes(bytes: Array[Byte], offset: Int): (SpecificBalances, Int) = {
    val (contractId, contractIdEnd) = BinarySerializer.parseShortByteStr(bytes, offset)
    val (assetIds, end)             = BinarySerializer.parseShortList(bytes, BinarySerializer.parseShortString, contractIdEnd)

    SpecificBalances(contractId, assetIds) -> end
  }
}
