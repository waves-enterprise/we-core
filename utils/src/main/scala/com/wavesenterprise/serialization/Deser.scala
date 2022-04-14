package com.wavesenterprise.serialization

import com.google.common.io.ByteArrayDataOutput
import com.google.common.io.ByteStreams.newDataOutput
import com.google.common.primitives.{Ints, Shorts}

//noinspection UnstableApiUsage
object Deser {

  def serializeBoolean(value: Boolean): Array[Byte] = {
    Array[Byte](if (value) 1 else 0)
  }

  def serializeArray(value: Array[Byte]): Array[Byte] = {
    val ndo = newDataOutput(Shorts.BYTES + value.length)
    writeArray(value, ndo)
    ndo.toByteArray
  }

  def writeArray(value: Array[Byte], output: ByteArrayDataOutput): Unit = {
    output.writeShort(value.length.ensuring(_.isValidShort))
    output.write(value)
  }

  def parseArraySize(bytes: Array[Byte], offset: Int = 0): (Array[Byte], Int) = {
    val length = Shorts.fromBytes(bytes(offset), bytes(offset + 1))
    val start  = offset + Shorts.BYTES
    val end    = start + length
    val result = bytes.slice(start, end)
    result -> end
  }

  def parseByteArrayOption(bytes: Array[Byte], length: Int, offset: Int = 0): (Option[Array[Byte]], Int) = {
    if (bytes(offset) == 1) {
      val start  = offset + 1
      val end    = start + length
      val result = bytes.slice(start, end)
      Some(result) -> end
    } else {
      None -> (offset + 1)
    }
  }

  def parseOption[T](bytes: Array[Byte], offset: Int = 0)(decoder: Array[Byte] => T): (Option[T], Int) = {
    if (bytes(offset) == 1) {
      val (arr, end) = parseArraySize(bytes, offset + 1)
      Some(decoder(arr)) -> end
    } else {
      None -> (offset + 1)
    }
  }

  def parseArrays(bytes: Array[Byte], offset: Int = 0): (Seq[Array[Byte]], Int) = {
    val length = Shorts.fromBytes(bytes(offset), bytes(offset + 1))
    val (result, end) = (1 to length).foldLeft((List.empty[Array[Byte]], offset + Shorts.BYTES)) {
      case ((acc, pos), _) =>
        val (arr, nextPos) = parseArraySize(bytes, pos)
        (arr :: acc, nextPos)
    }

    result.reverse -> end
  }

  def serializeOption[T](maybeT: Option[T])(encoder: T => Array[Byte]): Array[Byte] = {
    maybeT.fold[Array[Byte]](Array(0)) { value =>
      val encoded = encoder(value)
      val ndo     = newDataOutput(1 + Shorts.BYTES + encoded.length)
      ndo.writeByte(1)
      writeArray(encoded, ndo)
      ndo.toByteArray
    }
  }

  def serializeArrays(values: Seq[Array[Byte]]): Array[Byte] = {
    val outputSize = Shorts.BYTES + values.foldLeft(0)(_ + Shorts.BYTES + _.length)
    val ndo        = newDataOutput(outputSize)
    writeArrays(values, ndo)
    ndo.toByteArray
  }

  def writeArrays(values: Seq[Array[Byte]], output: ByteArrayDataOutput): Unit = {
    output.writeShort(values.length.ensuring(_.isValidShort))
    values.foreach(writeArray(_, output))
  }

  def serializeBigArray(value: Array[Byte]): Array[Byte] = {
    val ndo = newDataOutput(Ints.BYTES + value.length)
    writeBigArray(value, ndo)
    ndo.toByteArray
  }

  def writeBigArray(value: Array[Byte], output: ByteArrayDataOutput): Unit = {
    output.writeInt(value.length.ensuring(_.isValidInt))
    output.write(value)
  }

  def parseBigArraySize(bytes: Array[Byte], offset: Int): (Array[Byte], Int) = {
    val length = Ints.fromByteArray(bytes.slice(offset, offset + Integer.BYTES))
    val start  = offset + Integer.BYTES
    val end    = start + length
    val result = bytes.slice(start, end)
    result -> end
  }
}
