package com.wavesenterprise.crypto

import cats.implicits.{catsStdInstancesForEither, catsSyntaxFlatMapOps}
import cats.syntax.either._
import com.wavesenterprise.account.Address
import com.wavesenterprise.crypto.internals._
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.JsonFileStorage
import play.api.libs.json.{Format, Json}

import java.io.File
import java.nio.file.Files
import java.security.cert.Certificate
import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

class WavesKeyStore(storageFolder: Option[File], password: Array[Char], chainId: Byte) extends KeyStore[WavesKeyPair](storageFolder, password) {

  import WavesKeyStore._

  def toAlias(publicKey: WavesPublicKey): String = {
    Address.fromPublicKey(publicKey.getEncoded, chainId, WavesAlgorithms).address
  }

  override def getKey(alias: String, pwd: Option[Array[Char]]): Either[CryptoError, WavesPrivateKey] = {
    getKeyPair(alias, pwd).map(_.getPrivate)
  }

  override def getPublicKey(alias: String): Either[CryptoError, WavesPublicKey] = {
    accountsCache
      .get(alias)
      .toRight(GenericError(s"Public key for $alias wasn't found"))
      .map(_.publicKey)
  }

  override def getKeyPair(alias: String, pwd: Option[Array[Char]]): Either[CryptoError, WavesKeyPair] = {
    accountsCache
      .get(alias)
      .toRight(GenericError(s"Key wasn't found"))
      .flatMap {
        case encrypted: CachedEncryptedEntry =>
          for {
            pass <- pwd.toRight(GenericError(s"Expected password"))
            pk <- encrypted
              .privateKey(pass)
              .leftMap(_ => GenericError(s"Wrong password"))
          } yield WavesKeyPair(pk, encrypted.publicKey)

        case plain: CachedPlainEntry =>
          Right(WavesKeyPair(plain.privateKey, plain.publicKey))
      }
      .leftMap(error => error.copy(message = s"Failed to get key for $alias: ${error.message}"))
  }

  def getCertificate(alias: String): Either[CryptoError, Certificate] = {
    Either.left(GenericError("IllegalState: getCertificate called for Waves crypto. There are no certificates for Curve25519."))
  }

  def getCertificateChain(alias: String): Either[CryptoError, Array[Certificate]] = {
    Either.left(GenericError("IllegalState: getCertificateChain called for Waves crypto. There are no certificates for Curve25519."))
  }

  override def aliases(): Seq[String] = {
    accountsCache.keys.toSeq
  }

  def containsAlias(alias: String): Either[CryptoError, Boolean] = {
    Either.right(accountsCache.keys.toSeq.contains(alias))
  }

  private val key = JsonFileStorage.prepareKey(new String(password))

  private var walletData: WalletFileData = {
    storageFolder.fold {
      throw new IllegalStateException("Wallet file is required for Waves crypto")
    } { file =>
      Either.cond(file.exists(), (), "File doesn't exist") >>
        Either.cond(Files.isReadable(file.toPath), (), "File has no read access rights") >>
        (if (file.length() > 0) {
           try {
             JsonFileStorage.load[WalletFileData](file.getCanonicalPath, Some(key))
           } catch {
             case NonFatal(cause) => throw new RuntimeException(s"Unexpected error while reading wallet file '$file'", cause)
           }
         } else {
           Right(WalletFileData(Seq.empty))
         }) match {
        case Right(data) => data
        case Left(err)   => throw new IllegalStateException(s"Failed to read wallet file '$file': $err")
      }
    }
  }

  private val l = new Object

  private def lock[T](f: => T): T = l.synchronized(f)

  private val accountsCache = {
    val entries = walletData.entries.map { entry =>
      val publicKey = WavesPublicKey(entry.publicKey.arr)
      toAlias(publicKey) -> CachedEntry(entry)
    }
    TrieMap(entries: _*)
  }

  private def save(): Unit =
    storageFolder.foreach(f => JsonFileStorage.save(walletData, f.getCanonicalPath, Some(key)))

  private def generateNewAccountWithoutSave(pwd: Option[Array[Char]]): Option[CachedEntry] = lock {
    val kp    = WavesAlgorithms.generateKeyPair()
    val alias = toAlias(kp.getPublic)

    if (!accountsCache.contains(alias)) {
      ByteStr.decodeBase58(kp.getPrivate.base58).toOption.flatMap { privateKey =>
        val pk        = pwd.map(pass => ByteStr(aes.encrypt(pass, privateKey.arr))).getOrElse(privateKey)
        val fileEntry = FileEntry(publicKey = ByteStr(kp.getPublic.getEncoded), privateKey = pk)
        val cached    = CachedEntry(fileEntry)
        accountsCache += (alias -> cached)
        walletData = walletData.copy(entries = walletData.entries :+ fileEntry)
        Some(cached)
      }
    } else None
  }

  override def generateAndStore(pwd: Option[Array[Char]]): Option[WavesKeyPair#PublicKey0] = lock {
    generateNewAccountWithoutSave(pwd).map { acc =>
      save()
      acc.publicKey
    }
  }

  override def additionalStorageValidation(): Unit = require(walletData.entries.nonEmpty, "Wallet can't be empty for Waves cryptography")
}

object WavesKeyStore {
  private case class WalletFileData(entries: Seq[FileEntry])
  private case class FileEntry(publicKey: ByteStr, privateKey: ByteStr)

  private implicit val entryFormat: Format[FileEntry]               = Json.format[FileEntry]
  private implicit val walletFileDataFormat: Format[WalletFileData] = Json.format[WalletFileData]

  private val aes = new AesEncryption

  private sealed trait CachedEntry {
    def entry: FileEntry
    def publicKey: WavesPublicKey = WavesPublicKey(entry.publicKey.arr)
  }

  private object CachedEntry {
    def apply(fileEntry: FileEntry): CachedEntry =
      if (fileEntry.privateKey.arr.length == WavesAlgorithms.KeyLength) CachedPlainEntry(fileEntry)
      else CachedEncryptedEntry(fileEntry)
  }

  private case class CachedEncryptedEntry(entry: FileEntry) extends CachedEntry {
    def privateKey(password: Array[Char]): Either[CryptoError, WavesPrivateKey] = {
      aes.decrypt(password, entry.privateKey.arr).map(WavesPrivateKey)
    }
  }

  private case class CachedPlainEntry(entry: FileEntry) extends CachedEntry {
    def privateKey: WavesPrivateKey = WavesPrivateKey(entry.privateKey.arr)
  }
}
