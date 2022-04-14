package com.wavesenterprise.crypto.internals

import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.io.ByteArrayInputStream
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class AesStreamSpec extends PropSpec with ScalaCheckPropertyChecks with Matchers {

  val genSomeBytes: Gen[Array[Byte]] = for {
    length    <- Gen.choose(32 * 1024, 1 * 1024 * 1024)
    dataBytes <- Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])
  } yield dataBytes

  val genChunkSize: () => Int = () => Gen.choose(512, 2048).sample.get
  val random                  = new Random()

  property("Encrypt and decrypt some data") {

    forAll(genSomeBytes) { data =>
      val chunkSize: Int = 128 * 1024

      val dataStream = new ByteArrayInputStream(data)
      val key        = new Array[Byte](16)
      random.nextBytes(key)

      val encryptor = AesStream.Encryptor(key, chunkSize)
      val decryptor = AesStream.Decryptor(key, chunkSize)

      val encryptedChunks = ArrayBuffer[Byte]()

      while (dataStream.available() != 0) {
        val chunk = dataStream.readNBytes(Math.min(genChunkSize(), dataStream.available()))
        encryptedChunks ++= encryptor(chunk)
      }
      encryptedChunks ++= encryptor.doFinal()

      val encryptedDataStream = new ByteArrayInputStream(encryptedChunks.toArray)
      val resultDecrypted     = ArrayBuffer[Byte]()

      while (encryptedDataStream.available() != 0) {
        val chunk = encryptedDataStream.readNBytes(Math.min(genChunkSize(), encryptedDataStream.available()))
        resultDecrypted ++= decryptor(chunk).explicitGet()
      }

      resultDecrypted ++= decryptor.doFinal().explicitGet()

      assertResult(data)(resultDecrypted.toArray)
    }
  }

  property("Decryptor falls on changing encrypted data") {
    forAll(genSomeBytes) { data =>
      val key = new Array[Byte](16)
      random.nextBytes(key)

      val chunkSize      = 128 * 1024
      val encryptor      = AesStream.Encryptor(key, chunkSize)
      val decryptor      = AesStream.Decryptor(key, chunkSize)
      val encrypted      = encryptor(data) ++ encryptor.doFinal()
      val changedByteIdx = random.nextInt(encrypted.length)

      encrypted(changedByteIdx) = (encrypted(changedByteIdx) + 1).toByte

      val result = for {
        decrypted      <- decryptor(encrypted)
        finalDecrypted <- decryptor.doFinal()
      } yield decrypted ++ finalDecrypted

      assertResult("Tag mismatch!")(result.left.get.message)
    }
  }

  property("Decryptor falls on lost of data chunks") {
    forAll(genSomeBytes) { data =>
      val key = new Array[Byte](16)
      random.nextBytes(key)

      val chunkSize = 1024

      val encryptor = AesStream.Encryptor(key, chunkSize)
      val decryptor = AesStream.Decryptor(key, chunkSize)

      val encrypted         = encryptor(data) ++ encryptor.doFinal()
      val encryptedWithLost = encrypted.take(chunkSize)

      val result = for {
        decrypted      <- decryptor(encryptedWithLost)
        finalDecrypted <- decryptor.doFinal()
      } yield decrypted ++ finalDecrypted

      assertResult("Decryption failed! Probably some data chunks was reordered or lost")(result.left.get.message)
    }
  }

  property("Decryptor falls on data chunks reordering") {
    forAll(genSomeBytes) { data =>
      val key = new Array[Byte](16)
      random.nextBytes(key)

      val chunkSize = 16 * 1024

      val encryptor = AesStream.Encryptor(key, chunkSize)
      val decryptor = AesStream.Decryptor(key, chunkSize)

      val encrypted          = encryptor(data) ++ encryptor.doFinal()
      val reorderedEncrypted = encrypted.slice(chunkSize, chunkSize * 2) ++ encrypted.take(chunkSize)

      val result = for {
        decrypted      <- decryptor(reorderedEncrypted)
        finalDecrypted <- decryptor.doFinal()
      } yield decrypted ++ finalDecrypted

      assertResult("Invalid chunk index, chunk processing order must be the same as in encryption")(result.left.get.message)
    }
  }

}
