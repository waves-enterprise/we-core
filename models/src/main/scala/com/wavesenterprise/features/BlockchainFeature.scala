package com.wavesenterprise.features

import enumeratum.values.{ShortEnum, ShortEnumEntry}

import scala.collection.immutable

sealed abstract class BlockchainFeature(val value: Short, val description: String) extends ShortEnumEntry {
  def id: Short = value
}

object BlockchainFeature extends ShortEnum[BlockchainFeature] {

  case object NG                                   extends BlockchainFeature(2, "NG Protocol")
  case object MassTransfer                         extends BlockchainFeature(3, "Mass Transfer Transaction")
  case object SmartAccounts                        extends BlockchainFeature(4, "Smart Accounts")
  case object DataTransaction                      extends BlockchainFeature(5, "Data Transaction")
  case object BurnAnyTokens                        extends BlockchainFeature(6, "Burn Any Tokens")
  case object FeeSwitch                            extends BlockchainFeature(7, "Fee Switch")
  case object SmartAssets                          extends BlockchainFeature(9, "Smart Assets")
  case object SmartAccountTrading                  extends BlockchainFeature(10, "Smart Account Trading")
  case object ConsensusFix                         extends BlockchainFeature(100, "Updated PoS")
  case object ContractsGrpcSupport                 extends BlockchainFeature(101, "Support of gRPC for Docker contracts")
  case object PoaOptimisationFix                   extends BlockchainFeature(119, "Performance optimisation for PoA")
  case object SponsoredFeesSupport                 extends BlockchainFeature(120, "Sponsored fees support")
  case object MinerBanHistoryOptimisationFix       extends BlockchainFeature(130, "Performance optimisation for miner ban history")
  case object AtomicTransactionSupport             extends BlockchainFeature(140, "Support of atomic transaction")
  case object ParallelLiquidBlockGenerationSupport extends BlockchainFeature(160, "Support of parallel generation of liquid block with micro-block")
  case object ContractValidationsSupport           extends BlockchainFeature(162, "Support of Docker contracts validation")
  case object MicroBlockInventoryV2Support         extends BlockchainFeature(173, "Support of micro-block inventory v2")
  case object PrivacyLargeObjectSupport            extends BlockchainFeature(180, "Support of privacy large object subsystem")
  case object ContractNativeTokenSupportAndPkiV1Support
      extends BlockchainFeature(1120, "Support of token operations for smart-contracts, PKI support v1 and REST-based smart-contract deprecation")

  // Will be supported Issue|Reissue|Burn|Lease|LeaseCancel|Data|MassTransfer|RegisterNode|CreateAlias|SponsorFee txs in atomic
  case object OtherTxTypesAtomicSupport extends BlockchainFeature(1122, "Atomic support for other transactions")
  // ContractLease/ContractCancelLease
  case object LeaseOpsForContractsSupport extends BlockchainFeature(1123, "Support of Lease/CancelLease operations for smart-contracts")

  case object ConfidentialDataInContractsSupport extends BlockchainFeature(1130, "Support of operating over confidential data in smart contracts")

  override def values: immutable.IndexedSeq[BlockchainFeature] = findValues

  val implemented: Set[Short] = values.view.map(_.id).toSet

  implicit val BlockchainFeatureOrdering: Ordering[BlockchainFeature] = Ordering.by(_.id)
}
