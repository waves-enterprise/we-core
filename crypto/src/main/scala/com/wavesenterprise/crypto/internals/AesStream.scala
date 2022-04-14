package com.wavesenterprise.crypto.internals

import com.wavesenterprise.crypto.internals.StreamCipher.{AbstractDecryptor, AbstractEncryptor}

import java.security.MessageDigest
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

object AesStream {

  private val CipherName = "AES/GCM/NoPadding"

  private val keySize = 16 // 256 bit

  private val random = WavesAlgorithms.createSecureRandomInstance()

  class Encryptor private (key: Array[Byte], chunkSize: Int) extends AbstractEncryptor(chunkSize) {
    private lazy val keySpec: SecretKeySpec = {
      var keyBytes           = key
      val sha: MessageDigest = MessageDigest.getInstance("MD5")
      keyBytes = sha.digest(keyBytes)
      keyBytes = java.util.Arrays.copyOf(keyBytes, keySize)
      new SecretKeySpec(keyBytes, "AES")
    }

    private var iv: Array[Byte] = _

    override protected def getIv: Array[Byte] = iv

    override protected lazy val ivLength: Int = 16

    override protected lazy val macLength: Int = 16

    override protected lazy val cipher: Cipher = Cipher.getInstance(CipherName)

    override protected def resetCipher(): Unit = {
      iv = genIv
      cipher.init(Cipher.ENCRYPT_MODE, keySpec, new GCMParameterSpec(128, iv)) // reset cipher state
    }

    private def genIv: Array[Byte] = {
      val iv = new Array[Byte](ivLength)
      random.nextBytes(iv)
      iv
    }

    override protected def encrypt(plainText: Array[Byte]): Array[Byte] = {
      cipher.doFinal(plainText)
    }
  }

  object Encryptor {

    /**
      * Warning: decryption with chunk size which not equal to used in encryption won't give same result.
      */
    def apply(key: Array[Byte], chunkSize: Int): Encryptor = new Encryptor(key, chunkSize)
  }

  class Decryptor private (key: Array[Byte], chunkSize: Int) extends AbstractDecryptor(chunkSize) {
    private lazy val keySpec: SecretKeySpec = {
      var keyBytes           = key
      val sha: MessageDigest = MessageDigest.getInstance("MD5")
      keyBytes = sha.digest(keyBytes)
      keyBytes = java.util.Arrays.copyOf(keyBytes, keySize)
      new SecretKeySpec(keyBytes, "AES")
    }

    override protected lazy val ivLength: Int = 16

    override protected lazy val macLength: Int = 16

    override protected val cipher: Cipher = Cipher.getInstance(CipherName)

    override def decrypt(): Array[Byte] = {
      val data      = buffer.array().dropRight(buffer.remaining())
      val iv        = data.take(ivLength)
      val encrypted = data.slice(ivLength, data.length)
      cipher.init(Cipher.DECRYPT_MODE, keySpec, new GCMParameterSpec(128, iv))
      val decrypted = cipher.doFinal(encrypted)
      isLastDecryptedFinal = decrypted.head == StreamCipher.FinalChunkByte
      decrypted
    }
  }

  object Decryptor {

    /**
      * Warning: decryption with chunk size which not equal to used in encryption won't give same result.
      */
    def apply(key: Array[Byte], chunkSize: Int): Decryptor = new Decryptor(key, chunkSize)
  }
}
