package com.wavesenterprise.serialization

import com.google.common.io.ByteArrayDataOutput
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesenterprise.account.{Address, AddressOrAlias}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.AtomicBadge
import com.wavesenterprise.transaction.smart.script.{Script, ScriptReader}
import com.wavesenterprise.transaction.transfer.ParsedTransfer

import java.nio.charset.StandardCharsets.UTF_8

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

  def parseScript(bytes: Array[Byte], offset: Offset = 0): (Script, Offset) = {
    val (scriptBytes, scriptEnd) = BinarySerializer.parseShortByteArray(bytes, offset)
    ScriptReader.fromBytesUnsafe(scriptBytes) -> scriptEnd
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

  def writeAtomicBadge(atomicBadge: AtomicBadge, output: ByteArrayDataOutput): Unit = {
    val writeableField = atomicBadge.trustedSender
    BinarySerializer.writeByteIterable(writeableField, addressWriter, output)
  }

  def parseAtomicBadge(bytes: Array[Byte], offset: Offset = 0): (AtomicBadge, Offset) = {
    val (readableField, end) = BinarySerializer.parseOption(bytes, addressReader, offset)
    AtomicBadge(readableField) -> end
  }

  def writeAddresses(addresses: Seq[Address], output: ByteArrayDataOutput): Unit =
    BinarySerializer.writeShortIterable(addresses, addressWriter, output)

  def parseAddresses(bytes: Array[Byte], offset: Offset = 0): (List[Address], Offset) = {
    val (readableField, end) = BinarySerializer.parseShortList(bytes, addressReader, offset)
    readableField -> end
  }

  def writeTransferBatch(batch: Seq[ParsedTransfer], output: ByteArrayDataOutput): Unit = {
    output.writeShort(batch.size)
    batch.foreach { transfer =>
      output.write(transfer.recipient.bytes.arr)
      output.writeLong(transfer.amount)
    }
  }

  def parseTransferBatch(bytes: Array[Byte], offset: Offset = 0): (List[ParsedTransfer], Offset) = {
    val (length, start) = shortCountReader(bytes, offset)

    val (reversedResult, end) = (1 to length).foldLeft((List.empty[ParsedTransfer], start)) {
      case ((acc, pos), _) =>
        val (addressOrAlias, nextPos) = AddressOrAlias.fromBytesUnsafe(bytes, pos)
        val amount                    = Longs.fromByteArray(bytes.slice(nextPos, nextPos + Longs.BYTES))
        (ParsedTransfer(addressOrAlias, amount) :: acc) -> (nextPos + Longs.BYTES)
    }

    reversedResult.reverse -> end
  }

  private def byteCountWriter(count: Int, output: ByteArrayDataOutput): Unit = {
    require(count.isValidByte)
    output.writeByte(count)
  }

  private def shortCountWriter(count: Int, output: ByteArrayDataOutput): Unit = {
    require(count.isValidShort)
    output.writeShort(count)
  }

  private def shortCountReader(data: Array[Byte], offset: Offset): (Int, Offset) = {
    val count = Shorts.fromBytes(data(offset), data(offset + 1))
    (count, offset + Shorts.BYTES)
  }

  private def intCountReader(data: Array[Byte], offset: Offset): (Int, Offset) = {
    val count = Ints.fromBytes(data(offset), data(offset + 1), data(offset + 2), data(offset + 3))
    (count, offset + Ints.BYTES)
  }

  private def addressReader(bytes: Array[Byte], pos: Int): (Address, Int) = {
    Address.fromBytesUnsafe(bytes.slice(pos, pos + Address.AddressLength)) -> (pos + Address.AddressLength)
  }

  private def addressWriter(value: Address, output: ByteArrayDataOutput): Unit = {
    output.write(value.bytes.arr)
  }
}
