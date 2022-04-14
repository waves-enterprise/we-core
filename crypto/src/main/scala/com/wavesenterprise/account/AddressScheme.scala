package com.wavesenterprise.account

abstract class AddressScheme {
  val chainId: Byte
  override def toString: String = s"AddressScheme($chainId)"
}

object AddressScheme {
  @volatile private var current: AddressScheme = DefaultAddressScheme

  def setAddressSchemaByte(char: Char): Unit = {
    current = new AddressScheme {
      override val chainId: Byte = char.toByte
    }
  }

  def getAddressSchema: AddressScheme = {
    current
  }
}

object DefaultAddressScheme extends AddressScheme {
  val chainId: Byte = 'T'.toByte
}
