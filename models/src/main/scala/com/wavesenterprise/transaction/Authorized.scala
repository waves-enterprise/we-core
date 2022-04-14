package com.wavesenterprise.transaction

import com.wavesenterprise.account.PublicKeyAccount

trait Authorized {
  def sender: PublicKeyAccount
}
