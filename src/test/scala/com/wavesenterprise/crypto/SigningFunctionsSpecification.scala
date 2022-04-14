package com.wavesenterprise.crypto

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.crypto

class SigningFunctionsSpecification extends PropSpec with ScalaCheckPropertyChecks with Matchers {

  property("signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte], message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val keyPair1 = crypto.generateKeyPair()
        val keyPair2 = crypto.generateKeyPair()

        val acc      = PrivateKeyAccount(keyPair1)
        val sig      = crypto.sign(acc.privateKey, message1)
        val rightKey = acc.publicKey.getEncoded
        crypto.verify(sig, message1, rightKey) should be(true)

        val wrongKey = PrivateKeyAccount(keyPair2).publicKey
        crypto.verify(sig, message1, wrongKey) shouldNot be(true)

        crypto.verify(sig, message2, rightKey) shouldNot be(true)
      }
    }
  }
}
