package com.wavesenterprise.transaction

trait VersionedTransaction extends Transaction {
  def version: Byte
}
