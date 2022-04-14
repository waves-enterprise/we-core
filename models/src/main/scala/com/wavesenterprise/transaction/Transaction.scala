package com.wavesenterprise.transaction

import com.wavesenterprise.features.BlockchainFeature
import com.wavesenterprise.serialization.{BytesSerializable, JsonSerializable}
import com.wavesenterprise.state._
import monix.eval.Coeval

import scala.collection.SortedSet

trait Transaction extends BytesSerializable with JsonSerializable {
  val id: Coeval[ByteStr]

  def builder: TransactionParser
  def feeAssetId: Option[AssetId] = None
  def fee: Long
  def timestamp: Long

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  val bodyBytes: Coeval[Array[Byte]]
  def checkedAssets: List[AssetId] = List.empty

  def requiredFeatures: SortedSet[BlockchainFeature] = SortedSet.empty

  def proofSourceBytes: Array[Byte] = bodyBytes()
}

object Transaction {

  type Type = Byte

  implicit def timestampOrdering[T <: Transaction]: Ordering[T] = Ordering[Long].on(_.timestamp)
}
