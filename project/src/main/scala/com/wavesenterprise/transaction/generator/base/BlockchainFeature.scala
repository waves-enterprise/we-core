package com.wavesenterprise.transaction.generator.base

import enumeratum._

import scala.collection.immutable

sealed trait BlockchainFeature extends EnumEntry {
  def seqCode: String = s"Seq(BlockchainFeature.$entryName)"
}

object BlockchainFeature extends Enum[BlockchainFeature] {

  case object SponsoredFeesSupport                      extends BlockchainFeature
  case object ContractsGrpcSupport                      extends BlockchainFeature
  case object ValidateContracts                         extends BlockchainFeature
  case object SmartAccounts                             extends BlockchainFeature
  case object SmartAccountTrading                       extends BlockchainFeature
  case object SmartAssets                               extends BlockchainFeature
  case object MassTransfer                              extends BlockchainFeature
  case object DataTransaction                           extends BlockchainFeature
  case object AtomicTransactionSupport                  extends BlockchainFeature
  case object ContractValidationsSupport                extends BlockchainFeature
  case object ContractNativeTokenSupportAndPkiV1Support extends BlockchainFeature
  case object ConfidentialDataInContractsSupport        extends BlockchainFeature
  case object OtherTxTypesAtomicSupport                 extends BlockchainFeature
  case object LeaseOpsForContractsSupport               extends BlockchainFeature
  case object WasmContractsSupport                      extends BlockchainFeature

  case class EvalFeature(override val seqCode: String) extends BlockchainFeature

  override def values: immutable.IndexedSeq[BlockchainFeature] = findValues
}
