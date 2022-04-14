package com.wavesenterprise.transaction

import com.wavesenterprise.serialization.Deser
import com.wavesenterprise.state._

import java.nio.charset.StandardCharsets.UTF_8

object DataTransactionEntryOps extends DataEntryOps {

  val MaxKeySize: Byte    = 100
  val MaxValueSize: Short = Short.MaxValue

  override def validate[T <: DataEntry[_]](dataEntry: T): Either[ValidationError, T] = {
    for {
      _ <- Either.cond(dataEntry.key.length <= MaxKeySize, dataEntry, KeyLengthError(MaxKeySize))
      _ <- dataEntry match {
        case bde: BinaryDataEntry => Either.cond(bde.value.arr.length <= MaxValueSize, dataEntry, ValueLengthError(MaxValueSize, bde))
        case sde: StringDataEntry => Either.cond(sde.value.getBytes(UTF_8).length <= MaxValueSize, dataEntry, ValueLengthError(MaxValueSize, sde))
        case _                    => Right(dataEntry)
      }
    } yield dataEntry
  }

  override protected def serializeBinary(bde: BinaryDataEntry): Array[Byte] = Deser.serializeArray(bde.value.arr)

  override protected def serializeString(sde: StringDataEntry): Array[Byte] = Deser.serializeArray(sde.value.getBytes(UTF_8))

  override protected def parseBinary(key: String, bytes: Array[Byte], p: Int): (BinaryDataEntry, Int) = {
    val (blob, p1) = Deser.parseArraySize(bytes, p + 1)
    (BinaryDataEntry(key, ByteStr(blob)), p1)
  }

  override protected def parseString(key: String, bytes: Array[Byte], p: Int): (StringDataEntry, Int) = {
    val (blob, p1) = Deser.parseArraySize(bytes, p + 1)
    (StringDataEntry(key, new String(blob, UTF_8)), p1)
  }
}
