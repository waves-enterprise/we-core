package com.wavesenterprise.account

import cats.implicits.{catsSyntaxEither, catsSyntaxEitherObject}
import com.wavesenterprise.crypto
import com.wavesenterprise.crypto.PublicKey
import com.wavesenterprise.crypto.internals.{CryptoError, InvalidPublicKey}
import com.wavesenterprise.utils.Base58
import monix.eval.Coeval
import play.api.libs.json.{Format, JsError, JsString, JsSuccess, Reads, Writes}

trait PublicKeyAccount {
  def publicKey: PublicKey

  override def equals(b: Any): Boolean = b match {
    case a: PublicKeyAccount => publicKey.getEncoded.sameElements(a.publicKey.getEncoded)
    case _                   => false
  }

  override def hashCode(): Int = publicKey.hashCode()

  lazy val publicKeyBase58: String   = Base58.encode(publicKey.getEncoded)
  override lazy val toString: String = this.toAddress.address
}

object PublicKeyAccount {

  private case class PublicKeyAccountImpl(publicKey: PublicKey) extends PublicKeyAccount

  def apply(publicKey: PublicKey): PublicKeyAccount = PublicKeyAccountImpl(publicKey)

  def apply(publicKey: Array[Byte]): PublicKeyAccount =
    PublicKeyAccount(PublicKey(publicKey))

  def fromSessionPublicKey(sessionPublicKey: Array[Byte]): PublicKeyAccount = {
    PublicKeyAccount(PublicKey.sessionKeyFromBytes(sessionPublicKey))
  }

  implicit class PublicKeyAccountExt(pk: PublicKeyAccount) {
    def toAddress(chainId: Byte): Address = Address.fromPublicKey(pk.publicKey.getEncoded, chainId)
    def toAddress: Address                = Address.fromPublicKey(pk.publicKey.getEncoded)
    def address: String                   = toAddress.address
  }

  def fromBytes(bytes: Array[Byte]): Either[CryptoError, PublicKeyAccount] = {
    (for {
      _ <- Either.cond(
        bytes.length == crypto.KeyLength || !crypto.strictKeyLength,
        (),
        s"Bad public key bytes length: expected: '${crypto.KeyLength}', found: '${bytes.length}'"
      )
      account <- Either
        .catchNonFatal(PublicKeyAccount(bytes))
        .leftMap { ex =>
          s"Unable to create public key: ${ex.getMessage}"
        }
    } yield account).leftMap(InvalidPublicKey)
  }

  def fromBase58String(s: String): Either[CryptoError, PublicKeyAccount] = {
    (for {
      _ <- Either.cond(
        s.length <= crypto.KeyStringLength || !crypto.strictKeyLength,
        (),
        s"Bad public key string length: expected: '${crypto.KeyStringLength}', found: '${s.length}'"
      )
      bytes   <- Base58.decode(s).toEither.leftMap(ex => s"Unable to decode base58: ${ex.getMessage}")
      account <- fromBytes(bytes).leftMap(_.message)
    } yield account).leftMap { err =>
      InvalidPublicKey(s"Can't create public key from string '$s': $err")
    }
  }

  implicit val PublicKeyAccountWrites: Writes[PublicKeyAccount] = acc => JsString(acc.publicKeyBase58)

  implicit val PublicKeyAccountReads: Reads[PublicKeyAccount] = {
    case JsString(s) => PublicKeyAccount.fromBase58String(s).fold(e => JsError(e.message), JsSuccess(_))
    case _           => JsError("Expected string value for public key value")
  }

  implicit val PublicKeyAccountFormat: Format[PublicKeyAccount] = Format(
    PublicKeyAccountReads,
    PublicKeyAccountWrites
  )

  implicit val LazyPublicKeyFormat: Format[Coeval[PublicKeyAccount]] =
    Format.invariantFunctorFormat.inmap(PublicKeyAccount.PublicKeyAccountFormat, Coeval.pure[PublicKeyAccount], _.apply())
}
