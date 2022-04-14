package com.wavesenterprise.crypto.internals

import com.wavesenterprise.crypto
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class AesEncryptionSpec extends PropSpec with ScalaCheckPropertyChecks with Matchers {
  val enc = new AesEncryption
  property("symmetric encryption works (using AesEncryption class directly)") {
    forAll { (password: String, wrongPassword: String, value: String) =>
      val encrypted = enc.encrypt(password.getBytes("UTF-8"), value.getBytes("UTF-8"))

      enc.decrypt(password.getBytes("UTF-8"), encrypted).explicitGet() sameElements value.getBytes("UTF-8") shouldBe true

      if (password != wrongPassword) {
        enc.decrypt(wrongPassword.getBytes("UTF-8"), encrypted) shouldBe 'left
      }

    }
  }

  property("encryption with Curve25519 keys works (using common crypto interface)") {
    forAll { data: String =>
      val aliceKey  = crypto.generateSessionKeyPair()
      val bobKey    = crypto.generateSessionKeyPair()
      val dataBytes = data.getBytes("UTF-8")

      val encryptedDataWithWrappedKey = crypto.encrypt(dataBytes, aliceKey.getPrivate, bobKey.getPublic).explicitGet()
      val decryptedData               = crypto.decrypt(encryptedDataWithWrappedKey, bobKey.getPrivate, aliceKey.getPublic).explicitGet()
      decryptedData should contain theSameElementsAs dataBytes
    }
  }
}
