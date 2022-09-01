package com.wavesenterprise.transaction.smart

import com.wavesenterprise.crypto
import com.wavesenterprise.lang.v1.BaseGlobal
import com.wavesenterprise.transaction.Transaction
import com.wavesenterprise.transaction.assets.exchange.Order
import shapeless.{:+:, CNil}

object BlockchainContext {
  type In = Transaction :+: Order :+: CNil
  val baseGlobal: BaseGlobal = crypto.rideContext
}
