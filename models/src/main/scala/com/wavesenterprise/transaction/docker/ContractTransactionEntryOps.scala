package com.wavesenterprise.transaction.docker

import com.wavesenterprise.serialization.Deser
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.ValidationError

import java.nio.charset.StandardCharsets.UTF_8

/**
  * Byte serialize and deserialize operations for [[com.wavesenterprise.state.DataEntry]] objects for docker contract transactions
  */
object ContractTransactionEntryOps extends DataEntryOps {

  val MaxKeySize: Int = 100 // in symbols

  override def validate[T <: DataEntry[_]](dataEntry: T): Either[ValidationError, T] =
    Either.cond(dataEntry.key.length <= MaxKeySize, dataEntry, KeyLengthError(MaxKeySize))

  override protected def serializeBinary(bde: BinaryDataEntry): Array[Byte] = Deser.serializeBigArray(bde.value.arr)

  override protected def serializeString(sde: StringDataEntry): Array[Byte] = Deser.serializeBigArray(sde.value.getBytes(UTF_8))

  override protected def parseBinary(key: String, bytes: Array[Byte], p: Int): (BinaryDataEntry, Int) = {
    val (blob, p1) = Deser.parseBigArraySize(bytes, p + 1)
    (BinaryDataEntry(key, ByteStr(blob)), p1)
  }

  override protected def parseString(key: String, bytes: Array[Byte], p: Int): (StringDataEntry, Int) = {
    val (blob, p1) = Deser.parseBigArraySize(bytes, p + 1)
    (StringDataEntry(key, new String(blob, UTF_8)), p1)
  }
}
