package com.wavesenterprise

import com.wavesenterprise.utils.Constants.base58Length

package object transaction {

  type AssetId = com.wavesenterprise.state.ByteStr
  val AssetIdLength: Int       = com.wavesenterprise.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type AuthorizedTransaction = Authorized with Transaction
}
