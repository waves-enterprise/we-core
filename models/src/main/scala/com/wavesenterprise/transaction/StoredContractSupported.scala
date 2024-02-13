package com.wavesenterprise.transaction

import com.wavesenterprise.docker.StoredContract

trait StoredContractSupported extends VersionedTransaction {
  def storedContract: StoredContract
}
