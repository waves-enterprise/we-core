package com.wavesenterprise.transaction

import com.wavesenterprise.account.PublicKeyAccount

trait AtomicInnerTransaction extends VersionedTransaction {
  def sender: PublicKeyAccount
  def atomicBadge: Option[AtomicBadge]
}
