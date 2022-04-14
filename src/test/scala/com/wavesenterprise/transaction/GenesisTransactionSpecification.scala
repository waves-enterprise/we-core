package com.wavesenterprise.transaction

import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.crypto
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.wavesenterprise.account.{PrivateKeyAccount, PublicKeyAccount}
import scala.util.Success

@Ignore
class GenesisTransactionSpecification extends PropSpec with ScalaCheckPropertyChecks with Matchers {

  val bytes                    = Base58.decode("5bYeT2sAERLbE9gALbgy9TUQQ2GJF3FEAsNnyPGX1aB7e7KaCnAGCGZPDp57aTeUAYopC2j1WBuoMFkkBNLH7KrV").get
  private val defaultRecipient = PublicKeyAccount(bytes)

  property("GenesisTransaction Signature should be the same") {
    val balance   = 457L
    val timestamp = 2398762345L
    val signature = GenesisTransaction.generateSignature(defaultRecipient.toAddress, balance, timestamp)
    val expected  = "4LMK7D12n1ugq2B6gC38L1kK8W3s9MommD2179pWrDUbihjMKbFJjXCnpcR7BMQC5WEFkYvvnZ15mhtkW6PZNU1A"
    val actual    = Base58.encode(signature)
    assert(actual == expected)
  }

  property("GenesisTransaction parse from Bytes should work fine") {
    val bytes = Base58.decode("5GoidXWjBfzuSA3UJSB6gG35evATUUWZd2YE5StyNTnKVc9bicyTKF2rCG").get

    val actualTransaction = GenesisTransaction.parseBytes(bytes)

    val balance             = 12345L
    val timestamp           = 1234567890L
    val expectedTransaction = GenesisTransaction.create(defaultRecipient.toAddress, balance, timestamp).explicitGet()

    actualTransaction shouldBe Success(expectedTransaction)
  }

  property("GenesisTransaction serialize/deserialize roundtrip") {
    forAll(Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray), Gen.posNum[Long], Gen.posNum[Long]) {
      (recipientSeed: Array[Byte], time: Long, amount: Long) =>
        val keyPair   = crypto.generateKeyPair()
        val recipient = PrivateKeyAccount(keyPair)
        val s         = GenesisTransaction.create(recipient.toAddress, amount, time)
        val source    = s.right.get
        val bytes     = source.bytes()
        val dest      = GenesisTransaction.parseBytes(bytes).get

        source should equal(dest)
    }
  }

}
