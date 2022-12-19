package com.wavesenterprise.crypto.internals

import java.nio.ByteBuffer
import javax.crypto.Cipher

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import cats.syntax.either._

/**
  * AbstractEncryptor and AbstractDecryptor allows to encrypt and decrypt large amount of data using buffer of specific size
  * (8mb by default)
  *
  * Structure of data chunk:
  * 1. iv - initialization vector for cipher, 16 bytes
  * 2. chunkStatus - byte which indicates is current chunk final(=0x01) or not(=0x00)
  * 3. chunkIndex - index of the chunk to check if chunks was reordered by someone
  * 4. encryptedData - encrypted data, n bytes
  * 5. chunkCount - overall count of data chunks to check if some data was lost, 16 bytes, appended to the end of chunk
  *
  */
object StreamCipher {
  val NonFinalChunkByte: Byte = 0x00
  val FinalChunkByte: Byte    = 0x01

  abstract class AbstractEncryptor protected (chunkSize: Int) {
    private val plainTextChunkSize   = chunkSize - macLength
    protected val buffer: ByteBuffer = ByteBuffer.allocate(plainTextChunkSize)

    private val bufferWithoutDataPos = ivLength + 1 + 4 // 1 byte for final byte indication, 4 bytes for chunk idx
    private var chunkIndex           = 0

    protected def ivLength: Int
    protected def macLength: Int
    protected def cipher: Cipher

    initNewChunk(NonFinalChunkByte)

    protected def resetCipher(): Unit

    protected def initNewChunk(firstByte: Byte): Unit = {
      resetCipher()
      buffer.position(0)
      buffer.put(getIv)
      buffer.put(firstByte)
      buffer.putInt(chunkIndex)
      chunkIndex += 1
    }

    protected def getIv: Array[Byte]

    protected def encrypt(plainText: Array[Byte]): Array[Byte]

    def apply(data: Array[Byte]): Array[Byte] = {
      val result = ArrayBuffer[Byte]()

      var dataPosition = 0

      while (dataPosition != data.length) {
        val bytesProcessed = Math.min(data.length - dataPosition, buffer.remaining())
        buffer.put(data.slice(dataPosition, dataPosition + bytesProcessed))
        dataPosition += bytesProcessed

        if (!buffer.hasRemaining) {
          val (iv, plainText) = buffer.array().splitAt(ivLength)
          val encrypted       = encrypt(plainText)
          result ++= (iv ++ encrypted)
          initNewChunk(NonFinalChunkByte)
        }
      }

      result.toArray
    }

    def doFinal(): Array[Byte] = {
      val chunkCount = if (buffer.remaining() >= 4) chunkIndex else chunkIndex + 1

      val result = if (buffer.remaining() >= 4) {
        buffer.put(16, FinalChunkByte)
        apply(ByteBuffer.allocate(4).putInt(chunkCount).array())
      } else {
        val encrypted = apply(ByteBuffer.allocate(4).putInt(chunkCount).array())
        buffer.put(16, FinalChunkByte)
        encrypted
      }

      if (buffer.position() > bufferWithoutDataPos) {
        val (iv, plainText) = buffer.array().dropRight(buffer.remaining()).splitAt(ivLength)
        result ++ iv ++ encrypt(plainText)
      } else {
        result
      }
    }

  }

  abstract class AbstractDecryptor protected (chunkSize: Int) {
    protected val buffer: ByteBuffer = ByteBuffer.allocate(chunkSize)

    private var chunkIndex                     = 0
    private val last4PlainDataBytesOfPrevChunk = ByteBuffer.allocate(4) // just added to not skip 'chunkCount' filed while decrypting
    protected var isLastDecryptedFinal         = false

    protected def ivLength: Int
    protected def macLength: Int

    protected def cipher: Cipher

    def decrypt(): Array[Byte]

    def nextChunk(): Unit = {
      chunkIndex += 1
      buffer.position(0)
    }

    def saveLast4PlainTextBytes(decrypted: Array[Byte]): Unit = {
      val plainTextLength = decrypted.length - 4 - 1
      last4PlainDataBytesOfPrevChunk.position(0)
      last4PlainDataBytesOfPrevChunk.put(decrypted.takeRight(Math.min(4, plainTextLength)))
    }

    def apply(data: Array[Byte]): Either[DecryptionError, Array[Byte]] = {
      def validateChunk(decrypted: Array[Byte]): Unit = {
        if (ByteBuffer.wrap(decrypted.slice(1, 5)).getInt() != chunkIndex) {
          throw new RuntimeException("Invalid chunk index, chunk processing order must be the same as in encryption")
        }
      }

      val result       = ArrayBuffer[Byte]()
      var dataPosition = 0

      Try {
        while (dataPosition != data.length) {
          if (!buffer.hasRemaining) {
            nextChunk()
          }

          val bytesProcessed = Math.min(data.length - dataPosition, buffer.remaining())
          buffer.put(data.slice(dataPosition, dataPosition + bytesProcessed))
          dataPosition += bytesProcessed

          if (!buffer.hasRemaining) {
            val decrypted = decrypt()
            validateChunk(decrypted)

            result ++= last4PlainDataBytesOfPrevChunk.array().take(last4PlainDataBytesOfPrevChunk.position()) ++
              decrypted.drop(5).dropRight(4)

            saveLast4PlainTextBytes(decrypted)
          }
        }

        result.toArray
      }.toEither.leftMap(ex => {
        DecryptionError(ex.getMessage, ex)
      })
    }

    def doFinal(): Either[CryptoError, Array[Byte]] = {

      def validate(bytes: Array[Byte]): Unit = {
        def expectedChunkCount() = {
          val chunkCountBytes = (last4PlainDataBytesOfPrevChunk.array() ++ bytes.drop(5)).takeRight(4)
          ByteBuffer
            .wrap(chunkCountBytes)
            .position(0)
            .getInt()
        }

        val actualChunkCount = chunkIndex + 1

        if (
          (bytes.nonEmpty && ByteBuffer.wrap(bytes.slice(1, 5)).getInt() != chunkIndex) ||
          !isLastDecryptedFinal ||
          actualChunkCount != expectedChunkCount()
        ) {
          throw new RuntimeException("Decryption failed! Probably some data chunks was reordered or lost")
        }
      }

      Try {
        if (buffer.position() != 0 && buffer.hasRemaining) {
          val decryptedData = decrypt()
          validate(decryptedData)
          (last4PlainDataBytesOfPrevChunk.array().take(last4PlainDataBytesOfPrevChunk.position()) ++
            decryptedData.drop(5)).dropRight(4)
        } else {
          val result = Array[Byte]()
          validate(result)
          result
        }
      }.toEither.leftMap(ex => {
        DecryptionError(ex.getMessage, ex)
      })

    }

  }

}
