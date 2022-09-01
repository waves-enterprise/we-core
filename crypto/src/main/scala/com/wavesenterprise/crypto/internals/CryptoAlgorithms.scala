package com.wavesenterprise.crypto.internals

import cats.implicits._
import com.wavesenterprise.certs.CertChain
import com.wavesenterprise.crypto.internals.pki.Models.ExtendedKeyUsage
import monix.eval.Coeval
import org.slf4j.{Logger, LoggerFactory}
import scorex.crypto.hash.{Blake2b256, Keccak256}
import scorex.crypto.signatures.{Curve25519, PrivateKey => PrivateKeyS, PublicKey => PublicKeyS, Signature => SignatureS}
import scorex.util.encode.Base58

import java.security.cert._
import java.security.{Provider, SecureRandom, KeyStore => JavaKeyStore, PublicKey => JavaPublicKey}
import java.util
import scala.annotation.tailrec
import scala.util.Try

trait CryptoAlgorithms[KP <: KeyPair] {
  type KeyPair0    = KP
  type PublicKey0  = KeyPair0#PublicKey0
  type PrivateKey0 = KeyPair0#PrivateKey0

  val DigestSize: Int
  val SignatureLength: Int
  val KeyLength: Int
  val SessionKeyLength: Int
  val WrappedStructureLength: Int

  val strictKeyLength: Boolean

  protected val log: Logger = LoggerFactory.getLogger(this.getClass)

  def pkiRequiredOids: Set[ExtendedKeyUsage]                   = Set.empty
  def crlCheckIsEnabled: Boolean                               = false
  def maybeTrustKeyStoreProvider: Option[Coeval[JavaKeyStore]] = None

  def generateKeyPair(): KeyPair0

  def generateSessionKey(): KeyPair0

  def publicKeyFromBytes(bytes: Array[Byte]): PublicKey0

  def wrapPublicKey(publicKey: JavaPublicKey): PublicKey0

  def sessionKeyFromBytes(bytes: Array[Byte]): PublicKey0

  def fastHash(input: Array[Byte]): Array[Byte]

  def secureHash(input: Array[Byte]): Array[Byte]

  def sign(privateKey: PrivateKey0, message: Array[Byte]): Array[Byte]

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: PublicKey0): Boolean

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    verify(signature, message, publicKeyFromBytes(publicKey))

  def verify(signature: Array[Byte], message: Array[Byte], certChain: CertChain, timestamp: Long): Either[CryptoError, Unit]

  def getCaCerts(fingerprints: List[String]): Either[CryptoError, List[X509Certificate]]

  def validateCertChain(certChain: CertChain, timestamp: Long): Either[CryptoError, Unit]

  def buildEncryptor(senderPrivateKey: PrivateKey0,
                     recipientPublicKey: PublicKey0,
                     chunkSize: Int): Either[CryptoError, (Array[Byte], StreamCipher.AbstractEncryptor)]

  def buildDecryptor(encryptedKeyInfo: Array[Byte],
                     recipientPrivateKey: PrivateKey0,
                     senderPublicKey: PublicKey0,
                     chunkSize: Int): Either[CryptoError, StreamCipher.AbstractDecryptor]

  def encrypt(data: Array[Byte], senderPrivateKey: PrivateKey0, recipientPublicKey: PublicKey0): Either[CryptoError, EncryptedForSingle]

  def encryptForMany(data: Array[Byte], senderKey: PrivateKey0, recipientPublicKeys: Seq[PublicKey0]): Either[CryptoError, EncryptedForMany]

  def decrypt(encryptedDataWithKey: EncryptedForSingle,
              recipientPrivateKey: PrivateKey0,
              senderPublicKey: PublicKey0): Either[CryptoError, Array[Byte]]

  def sslProvider: Option[Provider]
}

case class EncryptedForSingle(encryptedData: Array[Byte], wrappedStructure: Array[Byte]) {

  override def hashCode(): Int =
    31 * util.Arrays.hashCode(encryptedData) + util.Arrays.hashCode(wrappedStructure)

  override def equals(a: Any): Boolean = a match {
    case other: EncryptedForSingle =>
      util.Arrays.equals(wrappedStructure, other.wrappedStructure) && util.Arrays.equals(encryptedData, other.encryptedData)
    case _ => false
  }

  override def toString: String = {
    val encryptedDataRepr = s"[${encryptedData.length} bytes]"
    val wrappedKeyRepr    = Base58.encode(wrappedStructure)
    s"EncryptedForSingle(encryptedData = $encryptedDataRepr, wrappedStructure = $wrappedKeyRepr)"
  }
}

/**
  * Holds data, encrypted on a single symmetrical key, with wrapped keys for a list of different recipients
  */
case class EncryptedForMany(encryptedData: Array[Byte], recipientPubKeyToWrappedKey: Map[PublicKey, Array[Byte]]) {

  override def hashCode(): Int = {
    31 * util.Arrays.hashCode(encryptedData) + recipientPubKeyToWrappedKey.hashCode()
  }

  override def equals(a: Any): Boolean = a match {
    case other: EncryptedForMany =>
      val keysAreEqual = recipientPubKeyToWrappedKey.forall {
        case (recipientPubKey, wrappedKey) =>
          other.recipientPubKeyToWrappedKey
            .get(recipientPubKey)
            .exists(otherWrapped => util.Arrays.equals(wrappedKey, otherWrapped))
      }
      keysAreEqual && util.Arrays.equals(encryptedData, other.encryptedData)
    case _ => false
  }

  /**
    * Rather big toString, use only for trace log messages
    */
  override def toString: String = {
    val encryptedDataRepr = s"[${encryptedData.length} bytes]"
    val recipientAddresses = {
      recipientPubKeyToWrappedKey
        .map {
          case (recipientPubKey, wrappedKey) =>
            "recipientPubKey: " + Base58.encode(recipientPubKey.getEncoded) + ", wrappedKey: " + Base58.encode(wrappedKey)
        }
        .mkString("; ")
    }
    s"EncryptedForMany(encryptedData = $encryptedDataRepr, recipientsToWrappedKey = $recipientAddresses)"
  }
}

object WavesAlgorithms extends CryptoAlgorithms[WavesKeyPair] {
  override val DigestSize: Int          = 32
  override val SignatureLength: Int     = 64
  override val KeyLength: Int           = 32
  override val SessionKeyLength: Int    = 32
  override val WrappedStructureLength   = 48
  override val strictKeyLength: Boolean = true
  private val secureRandom              = createSecureRandomInstance()
  private val aesEncryption             = new AesEncryption

  private val pkiNotSupportedError = PkiError("Not implemented for Waves crypto algorithms")

  override def generateKeyPair(): WavesKeyPair = {
    val (_, pair) = generateKeyPairWithSeed()
    pair
  }

  override def generateSessionKey(): WavesKeyPair = generateKeyPair()

  override def publicKeyFromBytes(bytes: Array[Byte]): WavesPublicKey = WavesPublicKey(bytes)

  override def wrapPublicKey(publicKey: JavaPublicKey): WavesPublicKey =
    publicKeyFromBytes(publicKey.getEncoded)

  override def sessionKeyFromBytes(bytes: Array[Byte]): PublicKey0 = publicKeyFromBytes(bytes)

  override def sign(privateKey: WavesPrivateKey, message: Array[Byte]): Array[Byte] = {
    Curve25519.sign(PrivateKeyS(privateKey.internal), message)
  }

  override def verify(signature: Array[Byte], message: Array[Byte], publicKey: WavesPublicKey): Boolean = {
    if (isWeakPublicKey(publicKey.getEncoded))
      false
    else
      Curve25519.verify(SignatureS(signature), message, PublicKeyS(publicKey.getEncoded))
  }

  override def verify(signature: Array[Byte], message: Array[Byte], certChain: CertChain, timestamp: Long): Either[CryptoError, Unit] =
    Left(pkiNotSupportedError)

  override def fastHash(input: Array[Byte]): Array[Byte] = Blake2b256.hash(input)

  override def secureHash(input: Array[Byte]): Array[Byte] = Keccak256.hash(Blake2b256.hash(input))

  /**
    * Encryption/Decryption is done via AES using Diffie-Hellman common secret.
    * A generated random symmetric key is used for encryption,
    * then it is encrypted on Diffie-Hellman secret of two participants.
    */
  def encrypt(data: Array[Byte], senderPrivateKey: WavesPrivateKey, recipientPublicKey: WavesPublicKey): Either[CryptoError, EncryptedForSingle] =
    Try {
      val symmetricKey        = aesEncryption.generateEncryptionKey()
      val secret: Array[Byte] = sharedSecret(senderPrivateKey, recipientPublicKey)

      val encryptedData = aesEncryption.encrypt(symmetricKey, data)
      val encryptedKey  = aesEncryption.encrypt(secret, symmetricKey)
      EncryptedForSingle(encryptedData, encryptedKey)
    }.toEither
      .leftMap { ex =>
        log.error("Error in encrypt", ex)
        GenericError("Error in encrypt")
      }

  /**
    * @return encrypted encryption key and encryptor - object which can be used for stream data encryption.
    */
  def buildEncryptor(senderPrivateKey: WavesPrivateKey,
                     recipientPublicKey: WavesPublicKey,
                     chunkSize: Int): Either[CryptoError, (Array[Byte], AesStream.Encryptor)] = {
    Try {
      val symmetricKey                   = aesEncryption.generateEncryptionKey()
      val secret: Array[Byte]            = sharedSecret(senderPrivateKey, recipientPublicKey)
      val encryptedKey: Array[Byte]      = aesEncryption.encrypt(secret, symmetricKey)
      val encryptor: AesStream.Encryptor = AesStream.Encryptor(symmetricKey, chunkSize)
      (encryptedKey, encryptor)
    }.toEither
      .leftMap { ex =>
        log.error("Error while building AES stream encryptor", ex)
        GenericError("Error while building AES stream encryptor")
      }
  }

  def encryptForMany(data: Array[Byte],
                     senderKey: WavesPrivateKey,
                     recipientPublicKeys: Seq[WavesPublicKey]): Either[CryptoError, EncryptedForMany] = {
    val symmetricKey  = aesEncryption.generateEncryptionKey()
    val encryptedData = aesEncryption.encrypt(symmetricKey, data)
    val recipientPubKeyToEncryptedKey = recipientPublicKeys.map { recipientPublicKey =>
      val secret: Array[Byte] = sharedSecret(senderKey, recipientPublicKey)
      val encryptedKey        = aesEncryption.encrypt(secret, symmetricKey)
      (recipientPublicKey: PublicKey) -> encryptedKey
    }.toMap

    Right(EncryptedForMany(encryptedData, recipientPubKeyToEncryptedKey))
  }

  def decrypt(encryptedDataWithKey: EncryptedForSingle,
              recipientPrivateKey: WavesPrivateKey,
              senderPublicKey: WavesPublicKey): Either[CryptoError, Array[Byte]] = {
    val EncryptedForSingle(encryptedData, wrappedKey) = encryptedDataWithKey
    for {
      secret <- Try(sharedSecret(recipientPrivateKey, senderPublicKey)).toEither
        .leftMap { ex =>
          log.error("Error in decrypt", ex)
          GenericError(s"Failed to make Diffie-Hellman shared secret")
        }
      symmetricKey <- aesEncryption.decrypt(secret, wrappedKey)
      data         <- aesEncryption.decrypt(symmetricKey, encryptedData)
    } yield data
  }

  /**
    * @param encryptedKeyInfo - encrypted encryption key
    * @return decryptor - object which can be used for stream data decryption
    */
  def buildDecryptor(encryptedKeyInfo: Array[Byte],
                     recipientPrivateKey: WavesPrivateKey,
                     senderPublicKey: WavesPublicKey,
                     chunkSize: Int): Either[CryptoError, AesStream.Decryptor] = {

    for {
      secret <- Try(sharedSecret(recipientPrivateKey, senderPublicKey)).toEither
        .leftMap { ex =>
          log.error("Error in decrypt", ex)
          GenericError(s"Failed to make Diffie-Hellman shared secret")
        }
      symmetricKey <- aesEncryption.decrypt(secret, encryptedKeyInfo)
    } yield AesStream.Decryptor(symmetricKey, chunkSize)
  }

  private def generateKeyPair(seed: Array[Byte]): WavesKeyPair = {
    val (privateKey, publicKey) = Curve25519.createKeyPair(seed)
    WavesKeyPair(WavesPrivateKey(privateKey), WavesPublicKey(publicKey))
  }

  private def generateKeyPairWithSeed(): (Array[Byte], WavesKeyPair) = {
    val MaxRetries = 100

    @tailrec
    def generateAndRetryOnWeak(retries: Int = 0): (Array[Byte], WavesKeyPair) = {
      if (retries > MaxRetries) {
        throw new IllegalStateException(s"Impossible situation! Failed to generate non-weak Curve25519 session key, number of retries: $retries")
      }
      val seed             = secureRandom.generateSeed(KeyLength)
      val generatedKeyPair = generateKeyPair(seed)
      if (isWeakPublicKey(generatedKeyPair.getPublic.getEncoded)) {
        generateAndRetryOnWeak(retries + 1)
      } else {
        seed -> generatedKeyPair
      }
    }

    generateAndRetryOnWeak()
  }

  private[crypto] def sharedSecret(privateKey: WavesPrivateKey, publicKey: WavesPublicKey): Array[Byte] = {
    Curve25519.createSharedSecret(PrivateKeyS(privateKey.internal), PublicKeyS(publicKey.getEncoded))
  }

  // see https://github.com/jedisct1/libsodium/blob/ab4ab23d5744a8e060864a7cec1a7f9b059f9ddd/src/libsodium/crypto_scalarmult/curve25519/ref10/x25519_ref10.c#L17
  private val blacklist: Array[Array[Byte]] = Array(
    // 0 (order 4)
    Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00),
    // 1 (order 1)
    Array(0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00),
    // 325606250916557431795983626356110631294008115727848805560023387167927233504 (order 8)
    Array(0xe0, 0xeb, 0x7a, 0x7c, 0x3b, 0x41, 0xb8, 0xae, 0x16, 0x56, 0xe3, 0xfa, 0xf1, 0x9f, 0xc4, 0x6a, 0xda, 0x09, 0x8d, 0xeb, 0x9c, 0x32, 0xb1,
      0xfd, 0x86, 0x62, 0x05, 0x16, 0x5f, 0x49, 0xb8, 0x00),
    // 39382357235489614581723060781553021112529911719440698176882885853963445705823 (order 8)
    Array(0x5f, 0x9c, 0x95, 0xbc, 0xa3, 0x50, 0x8c, 0x24, 0xb1, 0xd0, 0xb1, 0x55, 0x9c, 0x83, 0xef, 0x5b, 0x04, 0x44, 0x5c, 0xc4, 0x58, 0x1c, 0x8e,
      0x86, 0xd8, 0x22, 0x4e, 0xdd, 0xd0, 0x9f, 0x11, 0x57),
    // p-1 (order 2)
    Array(0xec, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f),
    // p (=0, order 4)
    Array(0xed, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f),
    // p+1 (=1, order 1)
    Array(0xee, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f)
  ).map(_.map(_.toByte))

  def isWeakPublicKey(publicKey: Array[Byte]): Boolean = {
    val c = Array.fill(7)(0)

    for (j <- publicKey.indices; i <- blacklist.indices)
      c(i) |= publicKey(j) ^ blacklist(i)(j)

    for (i <- blacklist.indices)
      c(i) |= (publicKey.last & 0x7f) ^ blacklist(i).last

    val k = c.foldLeft(0)((acc, b) => acc | (0xff & b) - 1)
    ((k >> 8) & 1) == 1
  }

  /**
    * Get an instance of SecureRandom
    * Windows doesn't have `NativePRNGNonBlocking`, so we'll have to use a default one
    */
  private[crypto] def createSecureRandomInstance(): SecureRandom = {
    Try(SecureRandom.getInstance("NativePRNGNonBlocking"))
      .orElse(Try(SecureRandom.getInstance("SHA1PRNG")))
      .getOrElse(SecureRandom.getInstanceStrong)
  }

  override def sslProvider: Option[Provider] = None

  override def getCaCerts(fingerprints: List[String]): Either[CryptoError, List[X509Certificate]] = Left(pkiNotSupportedError)

  override def validateCertChain(certChain: CertChain, timestamp: Long): Either[CryptoError, Unit] = Left(pkiNotSupportedError)
}
