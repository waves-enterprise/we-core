package com.wavesenterprise.transaction

import com.wavesenterprise.transaction.docker.assets.ContractTransferInV1

trait PaymentsV1ToContract {
  def payments: List[ContractTransferInV1]
}
