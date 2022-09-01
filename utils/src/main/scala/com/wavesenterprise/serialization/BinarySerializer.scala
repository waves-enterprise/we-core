package com.wavesenterprise.serialization

import com.google.common.io.ByteArrayDataOutput
import com.google.common.primitives.{Ints, Shorts}
import com.wavesenterprise.state.ByteStr

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.security.cert.{CertificateFactory, X509Certificate}

object BinarySerializer {

  type Offset    = Int
  type Writer[T] = (T, ByteArrayDataOutput) => Unit
  type Reader[T] = (Array[Byte], Offset) => (T, Offset)

  def writeIterable[T](
      iterable: Iterable[T],
      writer: Writer[T],
      countWriter: Writer[Int],
      output: ByteArrayDataOutput
  ): Unit = {
    countWriter(iterable.size, output)
    iterable.foreach(writer(_, output))
  }

  @inline
  def writeByteIterable[T](
      iterable: Iterable[T],
      writer: Writer[T],
      output: ByteArrayDataOutput
  ): Unit = writeIterable(iterable, writer, byteCountWriter, output)

  @inline
  def writeShortIterable[T](
      iterable: Iterable[T],
      writer: Writer[T],
      output: ByteArrayDataOutput
  ): Unit = writeIterable(iterable, writer, shortCountWriter, output)

  @inline
  def writeBigIterable[T](
      iterable: Iterable[T],
      writer: Writer[T],
      output: ByteArrayDataOutput
  ): Unit = writeIterable(iterable, writer, (count, out) => out.writeInt(count), output)

  def parseIterable[T](
      bytes: Array[Byte],
      countReader: Reader[Int],
      reader: Reader[T],
      offset: Offset = 0
  ): (List[T], Offset) = {
    val (count, countEnd) = countReader(bytes, offset)
    val (items, end) = (0 until count).foldLeft(List.empty[T] -> countEnd) {
      case ((acc, pos), _) =>
        val (item, nextPos) = reader(bytes, pos)
        (item :: acc, nextPos)
    }

    items.reverse -> end
  }

  @inline
  def parseShortList[T](
      bytes: Array[Byte],
      reader: Reader[T],
      offset: Offset = 0
  ): (List[T], Offset) = parseIterable(bytes, shortCountReader, reader, offset)

  @inline
  def parseBigList[T](
      bytes: Array[Byte],
      reader: Reader[T],
      offset: Offset = 0
  ): (List[T], Offset) = parseIterable(bytes, intCountReader, reader, offset)

  def parseOption[T](
      bytes: Array[Byte],
      reader: Reader[T],
      offset: Offset = 0
  ): (Option[T], Offset) = {
    val (nonEmpty, pos) = (bytes(offset), offset + 1)

    if (nonEmpty == 0) {
      None -> pos
    } else {
      val (value, end) = reader(bytes, pos)
      Some(value) -> end
    }
  }

  def writeShortByteArray(arr: Array[Byte], output: ByteArrayDataOutput): Unit = {
    output.writeShort(arr.length.ensuring(_.isValidShort))
    output.write(arr)
  }

  def parseShortByteArray(bytes: Array[Byte], offset: Offset = 0): (Array[Byte], Offset) = {
    val (length, start) = shortCountReader(bytes, offset)
    val end             = start + length
    val result          = bytes.slice(start, end)
    result -> end
  }

  @inline
  def writeShortString(s: String, output: ByteArrayDataOutput): Unit = {
    BinarySerializer.writeShortByteArray(s.getBytes(UTF_8), output)
  }

  def parseShortString(bytes: Array[Byte], offset: Offset = 0): (String, Offset) = {
    val (stringBytes, stringEnd) = BinarySerializer.parseShortByteArray(bytes, offset)
    new String(stringBytes, UTF_8) -> stringEnd
  }

  @inline
  def writeShortByteStr(s: ByteStr, output: ByteArrayDataOutput): Unit = {
    writeShortByteArray(s.arr, output)
  }

  def parseShortByteStr(bytes: Array[Byte], offset: Offset = 0): (ByteStr, Offset) = {
    val (strBytes, strEnd) = BinarySerializer.parseShortByteArray(bytes, offset)
    ByteStr(strBytes) -> strEnd
  }

  def writeBigByteArray(arr: Array[Byte], output: ByteArrayDataOutput): Unit = {
    output.writeInt(arr.length)
    output.write(arr)
  }

  def parseBigByteArray(bytes: Array[Byte], offset: Offset = 0): (Array[Byte], Offset) = {
    val (length, start) = intCountReader(bytes, offset)
    val end             = start + length
    val result          = bytes.slice(start, end)
    result -> end
  }

  def writeX509Cert(value: X509Certificate, output: ByteArrayDataOutput): Unit =
    writeShortByteArray(value.getEncoded, output)

  def parseX509Cert(bytes: Array[Byte], offset: Offset = 0): (X509Certificate, Offset) = {
    val (certBytes, end) = parseShortByteArray(bytes, offset)
    x509CertFromBytes(certBytes) -> end
  }

  def x509CertFromBytes(certBytes: Array[Byte]): X509Certificate = {
    val factory = CertificateFactory.getInstance("X.509")
    val cert    = factory.generateCertificate(new ByteArrayInputStream(certBytes))
    cert.asInstanceOf[X509Certificate]
  }

  private[serialization] def byteCountWriter(count: Int, output: ByteArrayDataOutput): Unit = {
    require(count.isValidByte)
    output.writeByte(count)
  }

  private[serialization] def shortCountWriter(count: Int, output: ByteArrayDataOutput): Unit = {
    require(count.isValidShort)
    output.writeShort(count)
  }

  private[serialization] def shortCountReader(data: Array[Byte], offset: Offset): (Int, Offset) = {
    val count = Shorts.fromBytes(data(offset), data(offset + 1))
    (count, offset + Shorts.BYTES)
  }

  private[serialization] def intCountReader(data: Array[Byte], offset: Offset): (Int, Offset) = {
    val count = Ints.fromBytes(data(offset), data(offset + 1), data(offset + 2), data(offset + 3))
    (count, offset + Ints.BYTES)
  }
}
