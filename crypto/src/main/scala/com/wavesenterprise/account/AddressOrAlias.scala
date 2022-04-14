package com.wavesenterprise.account

import com.wavesenterprise.crypto.internals.{CryptoError, InvalidAddress}
import com.wavesenterprise.serialization.Deser
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.EitherUtils.EitherExt

trait AddressOrAlias {
  def stringRepr: String

  def bytes: ByteStr

  override def toString: String = stringRepr

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: AddressOrAlias => bytes == a.bytes
    case _                 => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object AddressOrAlias {

  def fromBytes(bytes: Array[Byte], position: Int): Either[CryptoError, (AddressOrAlias, Int)] = {
    bytes(position) match {
      case Address.AddressVersion =>
        val addressEnd   = position + Address.AddressLength
        val addressBytes = bytes.slice(position, addressEnd)
        Address.fromBytes(addressBytes).map((_, addressEnd))
      case Alias.AddressVersion =>
        val (_, aliasEnd) = Deser.parseArraySize(bytes, position + 2)
        Alias.fromBytes(bytes.slice(position, aliasEnd)).map(_ -> aliasEnd)
      case x =>
        Left(InvalidAddress(s"Unknown address/alias version $x"))
    }
  }

  def fromBytesUnsafe(bytes: Array[Byte], offset: Int): (AddressOrAlias, Int) = {
    fromBytes(bytes, offset).explicitGet()
  }

  def fromString(s: String): Either[CryptoError, AddressOrAlias] = {
    if (s.startsWith(Alias.Prefix))
      Alias.fromString(s)
    else Address.fromString(s)
  }
}
