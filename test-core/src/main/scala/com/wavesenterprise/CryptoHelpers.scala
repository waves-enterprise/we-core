package com.wavesenterprise

import com.wavesenterprise.account.PrivateKeyAccount

object CryptoHelpers {

  /**
    * Use me only for unit tests!
    */
  def generatePrivateKey: PrivateKeyAccount = {
    PrivateKeyAccount(crypto.generateKeyPair())
  }
}
