package com.wavesenterprise.serialization

import com.google.common.io.ByteArrayDataOutput
import com.google.common.primitives.Longs
import com.wavesenterprise.account.{Address, AddressOrAlias}
import com.wavesenterprise.serialization.BinarySerializer._
import com.wavesenterprise.transaction.AtomicBadge
import com.wavesenterprise.transaction.smart.script.{Script, ScriptReader}
import com.wavesenterprise.transaction.transfer.ParsedTransfer

object ModelsBinarySerializer {

  def parseScript(bytes: Array[Byte], offset: Offset = 0): (Script, Offset) = {
    val (scriptBytes, scriptEnd) = BinarySerializer.parseShortByteArray(bytes, offset)
    ScriptReader.fromBytesUnsafe(scriptBytes) -> scriptEnd
  }

  def writeAtomicBadge(atomicBadge: AtomicBadge, output: ByteArrayDataOutput): Unit = {
    val writeableField = atomicBadge.trustedSender
    BinarySerializer.writeByteIterable(writeableField, addressWriter, output)
  }

  def parseAtomicBadge(bytes: Array[Byte], offset: Offset = 0): (AtomicBadge, Offset) = {
    val (readableField, end) = BinarySerializer.parseOption(bytes, addressReader, offset)
    AtomicBadge(readableField) -> end
  }

  def writeAddresses(addresses: Iterable[Address], output: ByteArrayDataOutput): Unit =
    BinarySerializer.writeShortIterable(addresses, addressWriter, output)

  def parseAddresses(bytes: Array[Byte], offset: Offset = 0): (List[Address], Offset) = {
    val (readableField, end) = BinarySerializer.parseShortList(bytes, addressReader, offset)
    readableField -> end
  }

  def parseAddressesSet(bytes: Array[Byte], offset: Offset = 0): (Set[Address], Offset) = {
    BinarySerializer.parseShortSet(bytes, addressReader, offset)
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

  private def addressReader(bytes: Array[Byte], pos: Int): (Address, Int) = {
    Address.fromBytesUnsafe(bytes.slice(pos, pos + Address.AddressLength)) -> (pos + Address.AddressLength)
  }

  private def addressWriter(value: Address, output: ByteArrayDataOutput): Unit =
    output.write(value.bytes.arr)

}
