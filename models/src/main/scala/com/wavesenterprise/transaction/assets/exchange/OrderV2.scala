package com.wavesenterprise.transaction.assets.exchange

import cats.data.State
import com.google.common.primitives.Longs
import com.wavesenterprise.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.crypto.KeyLength
import com.wavesenterprise.serialization.Deser
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction._
import monix.eval.Coeval

import scala.util.Try

/**
  * Order to matcher service for asset exchange
  */
case class OrderV2(senderPublicKey: PublicKeyAccount,
                   matcherPublicKey: PublicKeyAccount,
                   assetPair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   timestamp: Long,
                   expiration: Long,
                   matcherFee: Long,
                   proofs: Proofs)
    extends Order {

  override def version: Byte = 2

  override def signature: Array[Byte] = proofs.proofs(0).arr

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    (version +: senderPublicKey.publicKey.getEncoded) ++ matcherPublicKey.publicKey.getEncoded ++
      assetPair.bytes ++ orderType.bytes ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
      Longs.toByteArray(matcherFee)
  )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ proofs.bytes())
}

object OrderV2 {
  private val AssetIdLength = 32

  def buy(sender: PrivateKeyAccount,
          matcher: PublicKeyAccount,
          pair: AssetPair,
          amount: Long,
          price: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long): Order = {
    val unsigned = OrderV2(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
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
           matcherFee: Long): Order = {
    val unsigned = OrderV2(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
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
            matcherFee: Long): Order = {
    val unsigned = OrderV2(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def parseBytes(bytes: Array[Byte]): Try[Order] = Try {
    val readByte: State[Int, Byte] = State { from =>
      (from + 1, bytes(from))
    }
    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }
    def readEnd[T](f: (Array[Byte], Int) => T): State[Int, T] = State { from =>
      (from, f(bytes, from))
    }
    def parse[T](f: (Array[Byte], Int, Int) => (T, Int), size: Int): State[Int, T] = State { from =>
      val (res, off) = f(bytes, size, from)
      (off, res)
    }
    val makeOrder = for {
      version <- readByte
      _ = if (version != 2) { throw new Exception(s"Incorrect order version: expect 2 but found $version") }
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
      maybeProofs   <- readEnd(Proofs.fromBytes)
    } yield {
      OrderV2(
        sender,
        matcher,
        AssetPair(amountAssetId.map(ByteStr(_)), priceAssetId.map(ByteStr(_))),
        OrderType(orderType),
        amount,
        price,
        timestamp,
        expiration,
        matcherFee,
        maybeProofs.right.get
      )
    }
    makeOrder.run(0).value._2
  }
}
