package com.wavesenterprise.state

import com.google.common.io.ByteArrayDataOutput
import com.google.common.primitives.{Longs, Shorts}
import com.wavesenterprise.serialization.Deser
import com.wavesenterprise.state.DataEntry.Type
import com.wavesenterprise.transaction.ValidationError

import java.nio.charset.StandardCharsets.UTF_8

/**
  * Abstract class for [[DataEntry]] serialize / deserialize operations
  */
abstract class DataEntryOps {

  def validate[T <: DataEntry[_]](dataEntry: T): Either[ValidationError, T]

  def toBytes(dataEntry: DataEntry[_]): Array[Byte] = {
    Array.concat(serializeKey(dataEntry.key), valueBytes(dataEntry))
  }

  def writeBytes(dataEntry: DataEntry[_], output: ByteArrayDataOutput): Unit = {
    writeKey(dataEntry.key, output)
    writeValueBytes(dataEntry, output)
  }

  def valueBytes(dataEntry: DataEntry[_]): Array[Byte] = {
    dataEntry match {
      case ide: IntegerDataEntry => Type.Integer.id.toByte +: Longs.toByteArray(ide.value)
      case bde: BooleanDataEntry => Array[Byte](Type.Boolean.id.toByte, if (bde.value) 1 else 0)
      case bde: BinaryDataEntry  => Type.Binary.id.toByte +: serializeBinary(bde)
      case sde: StringDataEntry  => Type.String.id.toByte +: serializeString(sde)
    }
  }

  protected def writeValueBytes(dataEntry: DataEntry[_], output: ByteArrayDataOutput): Unit = {
    dataEntry match {
      case ide: IntegerDataEntry =>
        output.writeByte(Type.Integer.id)
        output.writeLong(ide.value)
      case bde: BooleanDataEntry =>
        output.writeByte(Type.Boolean.id)
        output.writeBoolean(bde.value)
      case bde: BinaryDataEntry =>
        output.writeByte(Type.Binary.id.toByte)
        output.write(serializeBinary(bde))
      case sde: StringDataEntry =>
        output.writeByte(Type.String.id)
        output.write(serializeString(sde))
    }
  }

  protected def serializeKey(key: String): Array[Byte] = Deser.serializeArray(key.getBytes(UTF_8))

  protected def writeKey(key: String, output: ByteArrayDataOutput): Unit = {
    Deser.writeArray(key.getBytes(UTF_8), output)
  }

  protected def parseKey(bytes: Array[Byte], p: Int): (String, Int) = {
    val keyLength = Shorts.fromBytes(bytes(p), bytes(p + 1))
    val start     = p + Shorts.BYTES
    val key       = new String(bytes, start, keyLength, UTF_8)
    key -> (start + keyLength)
  }

  protected def serializeBinary(bde: BinaryDataEntry): Array[Byte]

  protected def serializeString(sde: StringDataEntry): Array[Byte]

  protected def parseBinary(key: String, bytes: Array[Byte], p: Int): (BinaryDataEntry, Int)

  protected def parseString(key: String, bytes: Array[Byte], p: Int): (StringDataEntry, Int)

  def parse(bytes: Array[Byte], p: Int): (DataEntry[_], Int) = {
    val (key, p1) = parseKey(bytes, p)
    parseValue(key, bytes, p1)
  }

  def parseValue(key: String, bytes: Array[Byte], p: Int): (DataEntry[_], Int) = {
    val start = p + 1
    bytes(p) match {
      case t if t == Type.Integer.id => (IntegerDataEntry(key, Longs.fromByteArray(bytes.slice(start, start + Longs.BYTES))), start + Longs.BYTES)
      case t if t == Type.Boolean.id => (BooleanDataEntry(key, bytes(start) != 0), start + 1)
      case t if t == Type.Binary.id  => parseBinary(key, bytes, p)
      case t if t == Type.String.id  => parseString(key, bytes, p)
      case t                         => throw new Exception(s"Unknown type $t")
    }
  }
}

object KeyLengthError {

  def apply(maxKeySize: Int): ValidationError.GenericError = ValidationError.GenericError(s"Key length exceeds max length $maxKeySize")
}

object ValueLengthError {

  def apply(maxValueSize: Int, dataEntry: DataEntry[_]): ValidationError.GenericError =
    ValidationError.GenericError(s"Length of ${dataEntry.`type`} value for key ${dataEntry.key} exceeds max length $maxValueSize")
}
