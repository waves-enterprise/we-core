package com.wavesenterprise.transaction.smart

import com.wavesenterprise.lang.{CommonGlobal, Global}
import com.wavesenterprise.transaction.Transaction
import com.wavesenterprise.transaction.assets.exchange.Order
import shapeless.{:+:, CNil}

object BlockchainContext {
  type In = Transaction :+: Order :+: CNil
  val baseGlobal: CommonGlobal = Global
}
