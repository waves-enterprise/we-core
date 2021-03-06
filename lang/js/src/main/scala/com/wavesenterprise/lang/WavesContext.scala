package com.wavesenterprise.lang

import com.wavesenterprise.lang.v1.traits.domain.{Ord, Recipient, Tx}
import com.wavesenterprise.lang.v1.traits.{DataType, Environment}
import shapeless.{:+:, CNil}

object WavesContext {
  val env = new Environment {
    override def height: Long                                         = impl.Environment.height
    override def chainId: Byte                                        = impl.Environment.chainId
    override def inputEntity: Tx :+: Ord :+: CNil                     = impl.Environment.inputEntity
    override def transactionById(id: Array[Byte]): Option[Tx]         = impl.Environment.transactionById(id)
    override def transactionHeightById(id: Array[Byte]): Option[Long] = impl.Environment.transactionHeightById(id)
    override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] =
      impl.Environment.data(addressOrAlias, key, dataType)
    override def resolveAlias(name: String): Either[String, Recipient.Address] = impl.Environment.resolveAddress(name)
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] =
      impl.Environment.accountBalanceOf(addressOrAlias, assetId)
  }
}
