package com.wavesenterprise.crypto.internals

import cats.syntax.either._

import java.nio.ByteBuffer
import java.security.MessageDigest
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import scala.util.Try

class AesEncryption {

  private val cypherName = "AES/GCM/NoPadding"

  private val random   = WavesAlgorithms.createSecureRandomInstance()
  private val ivLength = 16
  private val keySize  = 16 // 256 bit

  def charsToBytes(chars: Array[Char]): Array[Byte] = {
    new String(chars).getBytes("UTF-8")
  }

  def encrypt(password: Array[Char], value: Array[Byte]): Array[Byte] = {
    encrypt(charsToBytes(password), value)
  }

  def encrypt(symmetricKey: Array[Byte], value: Array[Byte]): Array[Byte] = {
    val iv = new Array[Byte](ivLength)
    random.nextBytes(iv)

    val encrypted = cipher(Cipher.ENCRYPT_MODE, symmetricKey, iv).doFinal(value)

    val byteBuffer = ByteBuffer
      .allocate(iv.length + encrypted.length)
      .put(iv)
      .put(encrypted)

    val res = byteBuffer.array()
    wipeMemory(iv, encrypted)
    res
  }

  def decrypt(password: Array[Char], encryptedValue: Array[Byte]): Either[CryptoError, Array[Byte]] = {
    decrypt(charsToBytes(password), encryptedValue)
  }

  def decrypt(symmetricKey: Array[Byte], encryptedValue: Array[Byte]): Either[CryptoError, Array[Byte]] =
    Try {
      val byteBuffer = ByteBuffer.wrap(encryptedValue)

      val iv = new Array[Byte](ivLength)
      byteBuffer.get(iv)

      val encrypted = new Array[Byte](byteBuffer.remaining())
      byteBuffer.get(encrypted)

      val res = cipher(Cipher.DECRYPT_MODE, symmetricKey, iv).doFinal(encrypted)
      wipeMemory(iv, encrypted)
      res
    }.toEither
      .leftMap { ex =>
        GenericError(s"Error in AES decrypt: ${ex.getMessage}")
      }

  def generateEncryptionKey(): Array[Byte] = {
    val encryptionKey = new Array[Byte](keySize)
    random.nextBytes(encryptionKey)
    encryptionKey
  }

  private def cipher(mode: Int, key: Array[Byte], iv: Array[Byte]): Cipher = {
    val cipher = Cipher.getInstance(cypherName)
    cipher.init(mode, keyToSpec(key), new GCMParameterSpec(128, iv))
    cipher
  }

  private def keyToSpec(key: Array[Byte]): SecretKeySpec = {
    var keyBytes           = key
    val sha: MessageDigest = MessageDigest.getInstance("MD5")
    keyBytes = sha.digest(keyBytes)
    keyBytes = java.util.Arrays.copyOf(keyBytes, keySize)
    new SecretKeySpec(keyBytes, "AES")
  }

  private def wipeMemory(array: Array[Byte]*): Unit = {
    array.foreach { arr =>
      java.util.Arrays.fill(arr, 0: Byte)
    }
  }
}
