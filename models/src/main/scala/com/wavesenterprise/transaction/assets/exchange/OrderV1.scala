package com.wavesenterprise.transaction.assets.exchange

import cats.data.State
import com.google.common.primitives.Longs
import com.wavesenterprise.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.crypto.{KeyLength, SignatureLength}
import com.wavesenterprise.serialization.Deser
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction._
import monix.eval.Coeval

import scala.util.Try

/**
  * Order to matcher service for asset exchange
  */
case class OrderV1(senderPublicKey: PublicKeyAccount,
                   matcherPublicKey: PublicKeyAccount,
                   assetPair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   timestamp: Long,
                   expiration: Long,
                   matcherFee: Long,
                   proofs: Proofs)
    extends Order
    with Signed {

  override def version: Byte = 1

  override def signature: Array[Byte] = proofs.proofs(0).arr

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    senderPublicKey.publicKey.getEncoded ++ matcherPublicKey.publicKey.getEncoded ++
      assetPair.bytes ++ orderType.bytes ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
      Longs.toByteArray(matcherFee)
  )

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature, bodyBytes(), senderPublicKey.publicKey))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ signature)
}

object OrderV1 {
  private val AssetIdLength = 32

  def apply(senderPublicKey: PublicKeyAccount,
            matcherPublicKey: PublicKeyAccount,
            assetPair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            signature: Array[Byte]): OrderV1 = {
    OrderV1(senderPublicKey,
            matcherPublicKey,
            assetPair,
            orderType,
            amount,
            price,
            timestamp,
            expiration,
            matcherFee,
            Proofs(Seq(ByteStr(signature))))
  }

  def buy(sender: PrivateKeyAccount,
          matcher: PublicKeyAccount,
          pair: AssetPair,
          amount: Long,
          price: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def sell(sender: PrivateKeyAccount,
           matcher: PublicKeyAccount,
           pair: AssetPair,
           amount: Long,
           price: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def apply(sender: PrivateKeyAccount,
            matcher: PublicKeyAccount,
            pair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def parseBytes(bytes: Array[Byte]): Try[OrderV1] = Try {
    val readByte: State[Int, Byte] = State { from =>
      (from + 1, bytes(from))
    }
    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }
    def parse[T](f: (Array[Byte], Int, Int) => (T, Int), size: Int): State[Int, T] = State { from =>
      val (res, off) = f(bytes, size, from)
      (off, res)
    }
    val makeOrder = for {
      sender        <- read(PublicKeyAccount.apply, KeyLength)
      matcher       <- read(PublicKeyAccount.apply, KeyLength)
      amountAssetId <- parse(Deser.parseByteArrayOption, AssetIdLength)
      priceAssetId  <- parse(Deser.parseByteArrayOption, AssetIdLength)
      orderType     <- readByte
      price         <- read(Longs.fromByteArray, 8)
      amount        <- read(Longs.fromByteArray, 8)
      timestamp     <- read(Longs.fromByteArray, 8)
      expiration    <- read(Longs.fromByteArray, 8)
      matcherFee    <- read(Longs.fromByteArray, 8)
      signature     <- read(identity, SignatureLength)
    } yield {
      OrderV1(
        sender,
        matcher,
        AssetPair(amountAssetId.map(ByteStr(_)), priceAssetId.map(ByteStr(_))),
        OrderType(orderType),
        amount,
        price,
        timestamp,
        expiration,
        matcherFee,
        signature
      )
    }
    makeOrder.run(0).value._2
  }
}
