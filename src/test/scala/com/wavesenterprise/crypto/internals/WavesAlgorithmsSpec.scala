package com.wavesenterprise.crypto.internals

import com.wavesenterprise.account.{Address, AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.crypto.WavesKeyStore
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.io.ByteArrayInputStream
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer

class WavesAlgorithmsSpec extends PropSpec with ScalaCheckPropertyChecks with Matchers {

  property("Shared secret for two keys is the same for private A + public B & public A + private B") {
    val accountA = PrivateKeyAccount(crypto.generateKeyPair())
    val accountB = PrivateKeyAccount(crypto.generateKeyPair())

    val agreementA = WavesAlgorithms.sharedSecret(accountA.privateKey.asInstanceOf[WavesPrivateKey], accountB.publicKey.asInstanceOf[WavesPublicKey])
    val agreementB = WavesAlgorithms.sharedSecret(accountB.privateKey.asInstanceOf[WavesPrivateKey], accountA.publicKey.asInstanceOf[WavesPublicKey])
    agreementA should contain theSameElementsAs agreementB
  }

  property("two Curve25519 keys' secret can be used as password for AES") {
    forAll(Gen.containerOfN[Array, Byte](553, Arbitrary.arbitrary[Byte])) { bytes =>
      val originalData = new String(bytes)
      val senderKey    = PrivateKeyAccount(crypto.generateSessionKeyPair())
      val recipientKey = PrivateKeyAccount(crypto.generateSessionKeyPair())

      val encryptedData = WavesAlgorithms
        .encrypt(originalData.getBytes("UTF-8"),
                 senderKey.privateKey.asInstanceOf[WavesPrivateKey],
                 recipientKey.publicKey.asInstanceOf[WavesPublicKey])
        .explicitGet()

      val decryptedData = WavesAlgorithms
        .decrypt(encryptedData, recipientKey.privateKey.asInstanceOf[WavesPrivateKey], senderKey.publicKey.asInstanceOf[WavesPublicKey])
        .explicitGet()

      new String(decryptedData, "UTF-8") shouldBe originalData
    }
  }

  property("Long passwords for key") {
    val file     = Some(Files.createTempFile("keystore", ".dat").toFile)
    val keyStore = new WavesKeyStore(file, Array.empty[Char], AddressScheme.getAddressSchema.chainId)
    forAll(Gen.asciiPrintableStr.suchThat(_.length > 100)) { password: String =>
      val pk = keyStore.generateAndStore(Some(password.toCharArray))
      val kp = keyStore.getKeyPair(PublicKeyAccount.apply(pk.get.asInstanceOf[crypto.PublicKey]).address, Some(password.toCharArray)).explicitGet()
      kp.getPublic shouldBe pk.get
    }
  }

  property("Encrypt for the sender's key") {
    val senderKeyPair = WavesAlgorithms.generateSessionKey()
    val data          = Gen.containerOfN[Array, Byte](512, Arbitrary.arbitrary[Byte]).sample.get

    val encryptedStuff  = WavesAlgorithms.encrypt(data, senderKeyPair.getPrivate, senderKeyPair.getPublic).explicitGet()
    val decryptedResult = WavesAlgorithms.decrypt(encryptedStuff, senderKeyPair.getPrivate, senderKeyPair.getPublic).explicitGet()

    decryptedResult should contain theSameElementsAs data
  }

  property("Encryption of 1Kb to 1Mb of data") {
    val genSomeBytes = for {
      length    <- Gen.choose(1024, 1 * 1024 * 1024)
      dataBytes <- Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])
    } yield dataBytes

    forAll(genSomeBytes) { bytes =>
      val alice = WavesAlgorithms.generateKeyPair()
      val bob   = WavesAlgorithms.generateKeyPair()

      val encryptedDataWithWrappedKey = WavesAlgorithms.encrypt(bytes, alice.getPrivate, bob.getPublic).explicitGet()
      val decryptedResult             = WavesAlgorithms.decrypt(encryptedDataWithWrappedKey, bob.getPrivate, alice.getPublic).explicitGet()

      decryptedResult should contain theSameElementsAs bytes
    }
  }

  property("Check equality for EncryptedForMany") {
    val recipientPublicKeys = List.fill(3)(WavesAlgorithms.generateSessionKey().getPublic)
    val senderDummyKeyPair  = WavesAlgorithms.generateSessionKey()
    val data                = Gen.containerOfN[Array, Byte](512, Arbitrary.arbitrary[Byte]).sample.get

    val encryptedStuff1 = WavesAlgorithms.encryptForMany(data, senderDummyKeyPair.getPrivate, recipientPublicKeys).explicitGet()
    val encryptedStuff2 = WavesAlgorithms.encryptForMany(data, senderDummyKeyPair.getPrivate, recipientPublicKeys.tail).explicitGet()

    encryptedStuff1.equals(encryptedStuff2) shouldBe false
    encryptedStuff1.hashCode() should not be encryptedStuff2.hashCode()
  }

  property("Encrypt with common encryption key for many recipients") {
    val recipientPublicToPrivate = List.fill(3)(WavesAlgorithms.generateSessionKey()).map(keyPair => keyPair.getPublic -> keyPair.getPrivate).toMap
    val recipientAddressToPrivate: Map[Address, WavesPrivateKey] = recipientPublicToPrivate.map {
      case (publicKey, privateKey) => Address.fromPublicKey(publicKey.getEncoded) -> privateKey
    }
    val senderDummyKeyPair = WavesAlgorithms.generateSessionKey()
    val data               = Gen.containerOfN[Array, Byte](512, Arbitrary.arbitrary[Byte]).sample.get

    val encrypted = WavesAlgorithms.encryptForMany(data, senderDummyKeyPair.getPrivate, recipientPublicToPrivate.keys.toSeq).explicitGet()

    encrypted.recipientPubKeyToWrappedKey.foreach {
      case (publicKey, wrappedStructure) =>
        val recipientPrivateKey = recipientAddressToPrivate(Address.fromPublicKey(publicKey.getEncoded))
        val decryptedData = WavesAlgorithms
          .decrypt(EncryptedForSingle(encrypted.encryptedData, wrappedStructure), recipientPrivateKey, senderDummyKeyPair.getPublic)
          .explicitGet()

        decryptedData should contain theSameElementsAs data
    }
  }

  property("Stream encrypt and decrypt") {
    val genSomeBytes: Gen[(Array[Byte], Int)] = for {
      length    <- Gen.choose(32 * 1024, 1 * 1024 * 1024)
      chunkSize <- Gen.choose(1, length)
      dataBytes <- Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])
    } yield dataBytes -> chunkSize
    val genChunkSize: () => Int = () => Gen.choose(512, 2048).sample.get

    forAll(genSomeBytes) {
      case (data, chunkSize) =>
        val sender    = WavesAlgorithms.generateKeyPair()
        val recipient = WavesAlgorithms.generateKeyPair()

        val (encryptedKey, encryptor) = WavesAlgorithms.buildEncryptor(sender.getPrivate, recipient.getPublic, chunkSize).explicitGet()
        val dataStream                = new ByteArrayInputStream(data)

        val encryptedChunks = ArrayBuffer[Byte]()

        while (dataStream.available() != 0) {
          val chunk = dataStream.readNBytes(Math.min(genChunkSize(), dataStream.available()))
          encryptedChunks ++= encryptor(chunk)
        }
        encryptedChunks ++= encryptor.doFinal()

        val finalEncrypted = EncryptedForSingle(encryptedChunks.toArray, encryptedKey)

        val decryptor =
          WavesAlgorithms.buildDecryptor(finalEncrypted.wrappedStructure, recipient.getPrivate, sender.getPublic, chunkSize).explicitGet()
        val encryptedDataStream = new ByteArrayInputStream(finalEncrypted.encryptedData)
        val resultDecrypted     = ArrayBuffer[Byte]()

        while (encryptedDataStream.available() != 0) {
          val chunk = encryptedDataStream.readNBytes(Math.min(genChunkSize(), encryptedDataStream.available()))
          resultDecrypted ++= decryptor(chunk).explicitGet()
        }

        resultDecrypted ++= decryptor.doFinal().explicitGet()

        assertResult(data)(resultDecrypted.toArray)
    }
  }

}
