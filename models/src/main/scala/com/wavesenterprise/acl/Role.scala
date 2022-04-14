package com.wavesenterprise.acl

import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.GenericError
import enumeratum._

import java.nio.charset.StandardCharsets
import scala.collection.immutable

sealed abstract class Role(val byte: Byte, val prefixS: String) extends EnumEntry {
  def prefix: Array[Byte] = prefixS.getBytes(StandardCharsets.UTF_8)

  override def toString: String = prefixS
}

/**
  * Role that must have at least one address.
  */
sealed trait NonEmptyRole extends Role

object Role extends Enum[Role] {
  case object Miner             extends Role(1, "miner") with NonEmptyRole
  case object Issuer            extends Role(2, "issuer")
  case object Dexer             extends Role(3, "dex")
  case object Permissioner      extends Role(4, "permissioner") with NonEmptyRole
  case object Blacklister       extends Role(5, "blacklister")
  case object Banned            extends Role(6, "banned")
  case object ContractDeveloper extends Role(7, "contract_developer")
  case object ConnectionManager extends Role(8, "connection_manager") with NonEmptyRole
  case object Sender            extends Role(9, "sender")
  case object ContractValidator extends Role(10, "contract_validator")

  def fromByte(byte: Byte): Either[GenericError, Role] = {
    byte match {
      case Miner.byte             => Right(Miner)
      case Issuer.byte            => Right(Issuer)
      case Dexer.byte             => Right(Dexer)
      case Permissioner.byte      => Right(Permissioner)
      case Blacklister.byte       => Right(Blacklister)
      case Banned.byte            => Right(Banned)
      case ContractDeveloper.byte => Right(ContractDeveloper)
      case ConnectionManager.byte => Right(ConnectionManager)
      case Sender.byte            => Right(Sender)
      case ContractValidator.byte => Right(ContractValidator)
      case unknownByte            => Left(ValidationError.GenericError(s"Permission.fromByte failure: unknown role byte '$unknownByte'"))
    }
  }

  def fromStr(str: String): Either[GenericError, Role] = {
    str match {
      case Miner.prefixS             => Right(Miner)
      case Issuer.prefixS            => Right(Issuer)
      case Dexer.prefixS             => Right(Dexer)
      case Permissioner.prefixS      => Right(Permissioner)
      case Blacklister.prefixS       => Right(Blacklister)
      case Banned.prefixS            => Right(Banned)
      case ContractDeveloper.prefixS => Right(ContractDeveloper)
      case ConnectionManager.prefixS => Right(ConnectionManager)
      case Sender.prefixS            => Right(Sender)
      case ContractValidator.prefixS => Right(ContractValidator)
      case unknownStr                => Left(ValidationError.GenericError(s"Permission.fromStr failure: unknown role name '$unknownStr"))
    }
  }

  override def values: immutable.IndexedSeq[Role] = findValues
}
