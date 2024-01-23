package com.wavesenterprise.transaction.wasm

import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.UnknownContractEngineError

trait WasmContractSupported {
  def callFunc: Option[String]

  def contractEngine: String
}

object WasmContractSupported {
  def validateContractEngine(engine: String): Either[ValidationError, Unit] = {
    Either.cond(engine == "docker" || engine == "wasm", (), UnknownContractEngineError)
  }
}
