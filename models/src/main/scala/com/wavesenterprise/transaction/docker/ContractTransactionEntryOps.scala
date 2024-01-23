package com.wavesenterprise.transaction.docker

import cats.implicits._
import com.google.common.io.ByteArrayDataOutput
import com.wavesenterprise.serialization.{BinarySerializer, Deser}
import com.wavesenterprise.state.ByteStr.ByteStrFormat
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.ValidationError
import play.api.libs.json.{Format, JsError, JsObject, JsString, JsSuccess, Reads, Writes}

import java.nio.charset.StandardCharsets.UTF_8

/**
  * Serialize and deserialize operations for [[com.wavesenterprise.state.DataEntry]] objects for docker contract transactions
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

  type DataEntryList = List[DataEntry[_]]

  object DataEntryList {
    def fromBytes(bytes: Array[Byte], offset: Int): (DataEntryList, Int) = {
      BinarySerializer.parseBigList(bytes, parse, offset)
    }

    def writeBytes(entries: DataEntryList, output: ByteArrayDataOutput): Unit = {
      BinarySerializer.writeBigIterable(entries, ContractTransactionEntryOps.writeBytes, output)
    }

    implicit val format: Format[DataEntryList] = Format(Reads.list, Writes.list)
  }

  case class DataEntryMap(
      mapping: Map[ByteStr, DataEntryList]
  )

  object DataEntryMap {
    def fromBytes(bytes: Array[Byte], offset: Int): (DataEntryMap, Int) = {
      val (contractCount, countOffset) = BinarySerializer.parseInt(bytes, offset)
      var readOffset                   = countOffset
      var readCount                    = 0
      val result                       = scala.collection.mutable.Buffer[(ByteStr, DataEntryList)]()
      while (readCount < contractCount) {
        val (contractId, listOffset) = BinarySerializer.parseShortByteStr(bytes, readOffset)
        val (dataEntries, newOffset) = DataEntryList.fromBytes(bytes, listOffset)
        result += (contractId -> dataEntries)
        readCount += 1
        readOffset = newOffset
      }
      DataEntryMap(result.toMap) -> readOffset
    }

    def writeBytes(entryMap: DataEntryMap, output: ByteArrayDataOutput): Unit = {
      BinarySerializer.writeInt(entryMap.mapping.size, output)
      entryMap.mapping.foreach {
        case (contractId, entries) =>
          BinarySerializer.writeShortByteStr(contractId, output)
          DataEntryList.writeBytes(entries, output)
      }
    }

    implicit val format: Format[DataEntryMap] = {
      Format(
        {
          case _ @JsObject(fields) =>
            fields.toList.map {
              case (stringKey, jsonValues) =>
                for {
                  contractId <- ByteStrFormat.reads(JsString(stringKey))
                  jsonValues <- DataEntryList.format.reads(jsonValues)
                } yield {
                  contractId -> jsonValues
                }
            }.traverse(_.asEither)
              .left.map(JsError.apply)
              .right.map(s => JsSuccess(DataEntryMap(s.toMap)))
              .fold(identity, identity)

          case _ => JsError("expected base58 -> list[DataEntry] object")
        },
        {
          map =>
            JsObject(map.mapping.toList
              .map(kv => kv._1.base58 -> DataEntryList.format.writes(kv._2)))
        }
      )
    }
  }
}
