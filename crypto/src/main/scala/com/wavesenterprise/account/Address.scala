package com.wavesenterprise.account

import cats.implicits._
import com.wavesenterprise.crypto.PublicKey
import com.wavesenterprise.crypto.internals.{CryptoAlgorithms, CryptoError, InvalidAddress, KeyPair}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.Constants.base58Length
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.utils.{Base58, Caches, ScorexLogging}
import play.api.libs.json._
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

import java.nio.ByteBuffer
import scala.language.existentials
import scala.util.Try

sealed trait Address extends AddressOrAlias {
  val bytes: ByteStr
  lazy val address: String    = bytes.base58
  lazy val stringRepr: String = address
}

object Address extends ScorexLogging {
  val Prefix: String = "address:"

  val AddressVersion: Byte = 1
  val ChecksumLength       = 4
  val HashLength           = 20
  val AddressLength        = 1 + 1 + HashLength + ChecksumLength
  val AddressStringLength  = base58Length(AddressLength)

  val AddressCacheSize = 1000

  private def scheme = AddressScheme.getAddressSchema

  private class AddressImpl(val bytes: ByteStr) extends Address

  private val addressCache = Caches.cache[(CryptoAlgorithms[_ <: KeyPair], Byte, ByteStr), Address](
    AddressCacheSize,
    key => {
      val (cryptoAlgorithms, chainId, publicKey) = key

      val publicKeyHash = cryptoAlgorithms.secureHash(publicKey.arr)
      val withoutChecksum = {
        ByteBuffer
          .allocate(1 + 1 + HashLength)
          .put(AddressVersion)
          .put(chainId)
          .put(publicKeyHash, 0, HashLength)
          .array()
      }
      val bytes = Array.concat(withoutChecksum, calcCheckSum(withoutChecksum, cryptoAlgorithms))
      new AddressImpl(ByteStr(bytes))
    }
  )

  def fromPublicKey(publicKey: Array[Byte],
                    chainId: Byte = scheme.chainId,
                    cryptoAlgorithms: CryptoAlgorithms[_ <: KeyPair] = com.wavesenterprise.crypto.algorithms): Address = {
    val key = (cryptoAlgorithms, chainId, ByteStr(publicKey))
    addressCache.get(key)
  }

  def fromPublicKey(publicKey: PublicKey): Address = {
    fromPublicKey(publicKey.getEncoded)
  }

  def fromBytes(addressBytes: Array[Byte],
                chainId: Byte = scheme.chainId,
                cryptoAlgorithms: CryptoAlgorithms[_] = com.wavesenterprise.crypto.algorithms): Either[CryptoError, Address] = {
    (for {
      version <- Try(addressBytes.head).toEither.leftMap(ex => ex.getMessage)
      network <- Try(addressBytes.tail.head).toEither.leftMap(ex => ex.getMessage)
      _       <- Either.cond(version == AddressVersion, (), s"Unknown address version: $version")
      _       <- Either.cond(network == chainId, (), s"Data from other network: expected: $chainId(${chainId.toChar}), actual: $network(${network.toChar})")
      _ <- Either.cond(addressBytes.length == Address.AddressLength,
                       (),
                       s"Wrong addressBytes length: expected: ${Address.AddressLength}, actual: ${addressBytes.length}")
      checkSum          = addressBytes.takeRight(ChecksumLength)
      checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength), cryptoAlgorithms)
      _ <- Either.cond(
        checkSum.sameElements(checkSumGenerated),
        (),
        s"Bad address checksum. Address is corrupted or generated using a different crypto (check 'node.crypto.type' config parameter)."
      )
    } yield new AddressImpl(ByteStr(addressBytes))).left.map(InvalidAddress)
  }

  def fromBytesUnsafe(
      addressBytes: Array[Byte],
      chainId: Byte = scheme.chainId,
      cryptoAlgorithms: CryptoAlgorithms[_] = com.wavesenterprise.crypto.algorithms
  ): Address = fromBytes(addressBytes, chainId, cryptoAlgorithms).explicitGet()

  def fromString(addressStr: String): Either[CryptoError, Address] = {
    fromString(addressStr, scheme.chainId)
  }

  def fromString(addressStr: String, chainId: Byte): Either[CryptoError, Address] = {
    val base58String = if (addressStr.startsWith(Prefix)) addressStr.drop(Prefix.length) else addressStr
    for {
      _ <- Either.cond(base58String.length <= AddressStringLength,
                       (),
                       InvalidAddress(s"Wrong address string length: max=$AddressStringLength, actual: ${base58String.length}"))
      byteArray <- Base58.decode(base58String).toEither.left.map(ex => InvalidAddress(s"Unable to decode base58: ${ex.getMessage}"))
      address   <- fromBytes(byteArray, chainId)
    } yield address
  }

  def fromStringWithCustomCrypto(addressStr: String, cryptoAlgorithms: CryptoAlgorithms[_], chainId: Byte): Either[CryptoError, Address] = {
    val base58String = if (addressStr.startsWith(Prefix)) addressStr.drop(Prefix.length) else addressStr
    for {
      _ <- Either.cond(base58String.length <= AddressStringLength,
                       (),
                       InvalidAddress(s"Wrong address string length: max=$AddressStringLength, actual: ${base58String.length}"))
      byteArray <- Base58.decode(base58String).toEither.left.map(ex => InvalidAddress(s"Unable to decode base58: ${ex.getMessage}"))
      address   <- fromBytes(byteArray, chainId, cryptoAlgorithms)
    } yield address
  }

  private def calcCheckSum(withoutChecksum: Array[Byte], cryptoAlgorithms: CryptoAlgorithms[_]): Array[Byte] = {
    cryptoAlgorithms.secureHash(withoutChecksum).take(ChecksumLength)
  }

  implicit val reads: Reads[Address] = {
    case JsString(value) => fromString(value).fold(error => JsError(error.message), JsSuccess(_))
    case _               => JsError("Expected string value")
  }

  implicit val configReader: ConfigReader[Address] =
    ConfigReader.fromNonEmptyString[Address] { value =>
      Address.fromString(value).leftMap(err => CannotConvert(value, "Address", err.message))
    }

  implicit val writes: Writes[Address] = address => JsString(address.stringRepr)
}
