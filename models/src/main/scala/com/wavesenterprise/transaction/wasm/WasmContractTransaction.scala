package com.wavesenterprise.transaction.wasm

import com.wavesenterprise.transaction.VersionedTransaction

trait WasmContractTransaction extends VersionedTransaction {
  def bytecode: Array[Byte]
  def bytecodeHash: String
}
