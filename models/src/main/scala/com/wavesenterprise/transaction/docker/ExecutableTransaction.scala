package com.wavesenterprise.transaction.docker

import com.wavesenterprise.state.{ByteStr, DataEntry}
import com.wavesenterprise.transaction.docker.assets.ContractTransferInV1
import com.wavesenterprise.transaction.{FastHashId, VersionedTransaction}
import play.api.libs.json.Writes

trait ExecutableTransaction extends FastHashId with VersionedTransaction {

  def contractId: ByteStr

  def txType: Long

  def params: List[DataEntry[_]] = List.empty

  def fee: Long

  def payments: List[ContractTransferInV1] = List.empty
}

object ExecutableTransaction {

  val MaxBytes: Int = 125 * 1024 // 125KB

  implicit val writes: Writes[ExecutableTransaction] = tx => tx.json()
}
