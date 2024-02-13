package com.wavesenterprise.transaction.docker

import com.wavesenterprise.transaction.VersionedTransaction

trait DockerContractTransaction extends VersionedTransaction {
  def image: String
  def imageHash: String
}
