package com.wavesenterprise.transaction

import com.wavesenterprise.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.transaction.transfer._
import org.scalatest.{Assertions, Matchers, PropSpec}

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  val keyPair = crypto.generateKeyPair()

  val addressString = PublicKeyAccount(crypto.generateKeyPair().getPublic).address

  property("Transaction ordering should sort txs by ascending block timestamp") {
    val correctSeq = Seq(
      TransferTransactionV2
        .selfSigned(PrivateKeyAccount(keyPair), None, None, 123L, 100000, 1, Address.fromString(addressString).explicitGet(), Array())
        .right
        .get,
      TransferTransactionV2
        .selfSigned(PrivateKeyAccount(keyPair), None, None, 124L, 100000, 1, Address.fromString(addressString).explicitGet(), Array())
        .right
        .get
    )
    Random.shuffle(correctSeq).sorted(Transaction.timestampOrdering) shouldBe correctSeq
  }
}
