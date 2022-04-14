package com.wavesenterprise.settings

import enumeratum.EnumEntry

import scala.collection.immutable

sealed trait BlockchainType extends EnumEntry

object BlockchainType extends enumeratum.Enum[BlockchainType] {

  case object MAINNET extends BlockchainType
  case object CUSTOM  extends BlockchainType
  case object DEFAULT extends BlockchainType

  val values: immutable.IndexedSeq[BlockchainType] = findValues
}
