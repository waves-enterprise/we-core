package com.wavesenterprise

import com.wavesenterprise.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.crypto.internals._
import com.wavesenterprise.settings.CryptoSettings
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.Constants.base58Length
import scorex.crypto.signatures.{MessageToSign, Signature, PublicKey => PublicKeyBytes}

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.security.Provider
import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}

package crypto {
  object CryptoInitializer {
    private[crypto] val cryptoSettingsPromise: Promise[CryptoSettings] = Promise()

    def init(cryptoSettings: CryptoSettings): Either[CryptoError, Unit] = {
      for {
        _ <- Either.cond(!cryptoSettingsPromise.isCompleted, (), GenericError("Initialization error: Crypto was already initialized!"))
      } yield {
        cryptoSettingsPromise.success(cryptoSettings)
      }
    }
  }
}

package object crypto {
  lazy val cryptoSettings: CryptoSettings = readEnvForCryptoSettings()
    .getOrElse(Await.result(CryptoInitializer.cryptoSettingsPromise.future, 7.seconds))

  def readEnvForCryptoSettings(): Option[CryptoSettings] = {
    val wavesCryptoSettingKey = "node.waves-crypto"
    val positiveValues        = List("yes", "true").map(_.toUpperCase)

    Option(System.getProperty(wavesCryptoSettingKey)).flatMap {
      case wavesCryptoSettingValue if positiveValues.contains(wavesCryptoSettingValue.toUpperCase) =>
        Some(CryptoSettings.WavesCryptoSettings)

      case unsupportedValue =>
        throw new IllegalArgumentException(s"Unacceptable value for parameter '$wavesCryptoSettingKey': '$unsupportedValue'")
    }
  }

  def toAlias(keyPair: com.wavesenterprise.crypto.internals.KeyPair,
              hashLength: Int,
              chainId: Byte,
              addressVersion: Byte,
              checksumLength: Int): String = {
    val publicKeyHash   = algorithms.secureHash(keyPair.getPublic.getEncoded).take(hashLength)
    val withoutChecksum = addressVersion +: chainId +: publicKeyHash
    val bytes           = withoutChecksum ++ calcCheckSum(withoutChecksum, checksumLength)
    ByteStr(bytes).base58
  }

  def calcCheckSum(withoutChecksum: Array[Byte], checksumLength: Int): Array[Byte] = {
    algorithms.secureHash(withoutChecksum).take(checksumLength)
  }

  lazy val context: CryptoContext = {
    cryptoSettings match {
      case CryptoSettings.WavesCryptoSettings => new WavesCryptoContext
    }
  }

  type KeyPair    = context.KeyPair0
  type PublicKey  = context.PublicKey0
  type PrivateKey = context.PrivateKey0

  val algorithms = context.algorithms

  def keyStore(file: Option[File], password: Array[Char]): KeyStore[KeyPair] =
    context.keyStore(file, password)

  object PublicKey {
    def apply(bytes: Array[Byte]): PublicKey               = algorithms.publicKeyFromBytes(bytes)
    def sessionKeyFromBytes(bytes: Array[Byte]): PublicKey = algorithms.sessionKeyFromBytes(bytes)
  }

  val DigestSize: Int             = algorithms.DigestSize
  val KeyLength: Int              = algorithms.KeyLength
  val KeyStringLength: Int        = base58Length(KeyLength)
  val SessionKeyLength: Int       = algorithms.SessionKeyLength
  val SignatureLength: Int        = algorithms.SignatureLength
  val WrappedStructureLength: Int = algorithms.WrappedStructureLength

  type Message = Array[Byte]
  type Digest  = Array[Byte]

  def generateKeyPair(): KeyPair =
    algorithms.generateKeyPair()

  def generateSessionKeyPair(): KeyPair =
    algorithms.generateSessionKey()

  def secureHash(input: Message): Digest =
    algorithms.secureHash(input)

  def secureHash(input: String): Digest =
    secureHash(input.getBytes(UTF_8))

  def fastHash(input: Message): Digest =
    algorithms.fastHash(input)

  def fastHash(input: String): Digest =
    fastHash(input.getBytes(UTF_8))

  def generatePublicKey: PublicKeyAccount = {
    PublicKeyAccount(generateKeyPair().getPublic)
  }

  def sign(privateKey: PrivateKey, message: Array[Byte]): Signature =
    Signature(algorithms.sign(privateKey, message))

  def sign(pka: PrivateKeyAccount, message: Array[Byte]): Signature =
    Signature(algorithms.sign(pka.privateKey, message))

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = {
    if (signature.length != SignatureLength) false
    else algorithms.verify(Signature(signature), message, PublicKeyBytes(publicKey))
  }

  def verify(signature: Array[Byte], message: MessageToSign, publicKey: PublicKey): Boolean =
    algorithms.verify(Signature(signature), message, publicKey)

  def encrypt(data: Array[Byte], senderPrivateKey: PrivateKey, recipientPublicKey: PublicKey): Either[CryptoError, EncryptedForSingle] =
    context.algorithms.encrypt(data, senderPrivateKey, recipientPublicKey)

  def encryptForMany(data: Array[Byte],
                     senderPrivateKey: PrivateKey,
                     recipientsPublicKeys: List[PublicKey]): Either[CryptoError, EncryptedForMany] = {

    algorithms.encryptForMany(data, senderPrivateKey, recipientsPublicKeys)
  }

  def decrypt(encryptedDataWithKey: EncryptedForSingle,
              recipientPrivateKey: PrivateKey,
              senderPublicKey: PublicKey): Either[CryptoError, Array[Byte]] = {

    algorithms.decrypt(encryptedDataWithKey, recipientPrivateKey, senderPublicKey)
  }

  // linear time equality comparison against time attacks
  def safeIsEqual(a1: Array[Byte], a2: Array[Byte]): Boolean =
    java.security.MessageDigest.isEqual(a1, a2)

  def sslProvider: Option[Provider] = algorithms.sslProvider
}
