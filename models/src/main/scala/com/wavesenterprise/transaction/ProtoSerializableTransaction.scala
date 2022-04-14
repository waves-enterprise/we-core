package com.wavesenterprise.transaction

import com.wavesenterprise.transaction.protobuf.{Transaction => PbTransaction}

trait ProtoSerializableTransaction extends Transaction {
  def toProto: PbTransaction
}
