package com.wavesenterprise.transaction.transfer

import com.google.common.primitives.Longs
import com.wavesenterprise.account.AddressOrAlias
import play.api.libs.json.{Format, Json}

case class TransferDescriptor(recipient: String, amount: Long)

object TransferDescriptor {
  implicit val format: Format[TransferDescriptor] = Json.format
}

case class ParsedTransfer(recipient: AddressOrAlias, amount: Long) {
  def toDescriptor: TransferDescriptor = TransferDescriptor(recipient.stringRepr, amount)
}

object ParsedTransfer {

  def fromBytes(bytes: Array[Byte], position: Int): (ParsedTransfer, Int) = {
    val (address, addressEnd) = AddressOrAlias.fromBytesUnsafe(bytes, position)
    val amount                = Longs.fromByteArray(bytes.slice(addressEnd, addressEnd + Longs.BYTES))
    ParsedTransfer(address, amount) -> (addressEnd + Longs.BYTES)
  }
}
