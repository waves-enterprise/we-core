package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.PrivateKeyAccount
import org.scalatest.Suite

trait WithSenderAndRecipient extends CoreTransactionGen { _: Suite =>
  val senderAccount: PrivateKeyAccount = accountGen.sample.get
  val senderPkBase58: String           = senderAccount.publicKeyBase58

  val recipientAccount: PrivateKeyAccount = accountGen.sample.get
  val recipientPkBase58: String           = recipientAccount.publicKeyBase58
}
