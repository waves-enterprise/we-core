package com.wavesenterprise.transaction.smart

import com.wavesenterprise.lang.v1.traits._
import com.wavesenterprise.lang.v1.traits.domain.{Ord, Recipient, Tx}
import com.wavesenterprise.transaction.ValidationError.GenericError
import shapeless._

class EmptyWavesEnvironment(nByte: Byte) extends Environment {
  override def height: Long                                                             = ???
  override def chainId: Byte                                                            = nByte
  override def inputEntity: Tx :+: Ord :+: CNil                                         = ???
  override def transactionById(id: Array[Byte]): Option[Tx]                             = None
  override def transactionHeightById(id: Array[Byte]): Option[Long]                     = None
  override def resolveAlias(name: String): Either[String, Recipient.Address]            = Left(GenericError("Empty blockchain")).left.map(_.toString)
  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] = None
  override def accountBalanceOf(addressOrAlias: Recipient, maybeAssetId: Option[Array[Byte]]): Either[String, Long] =
    Left(GenericError("Empty blockchain")).left.map(_.toString)
}
