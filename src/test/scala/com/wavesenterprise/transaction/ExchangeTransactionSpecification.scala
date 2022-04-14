package com.wavesenterprise.transaction

import com.wavesenterprise.OrderOps._
import com.wavesenterprise.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.ValidationError.OrderValidationError
import com.wavesenterprise.transaction.assets.exchange.{Order, _}
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.{CoreTransactionGen, NTPTime, NoShrink}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

import scala.math.pow

class ExchangeTransactionSpecification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient
    with NTPTime
    with NoShrink {

  property("ExchangeTransaction transaction serialization roundtrip") {
    forAll(exchangeTransactionGen) { om =>
      val recovered = ExchangeTransaction.parse(om.bytes()).get
      om.id() shouldBe recovered.id()
      om.buyOrder.idStr() shouldBe recovered.buyOrder.idStr()
      recovered.bytes() shouldEqual om.bytes()
    }
  }

  property("ExchangeTransaction balance changes") {
    val versionsGen: Gen[(Byte, Byte)] = Gen.oneOf((1: Byte, 1: Byte), (1: Byte, 2: Byte), (2: Byte, 1: Byte), (2: Byte, 2: Byte))
    forAll(accountGen, accountGen, accountGen, assetPairGen, versionsGen) {
      case (sender1, sender2, matcher, pair, versions) =>
        val time                = ntpTime.correctedTime()
        val expirationTimestamp = time + Order.MaxLiveTime
        val buyPrice            = 60 * Order.PriceConstant
        val sellPrice           = 50 * Order.PriceConstant
        val buyAmount           = 2
        val sellAmount          = 3
        val mf1                 = 1
        val mf2                 = 2
        val (o1ver, o2ver)      = versions

        val buy  = Order.buy(sender1, matcher, pair, buyAmount, buyPrice, time, expirationTimestamp, mf1, o1ver)
        val sell = Order.sell(sender2, matcher, pair, sellAmount, sellPrice, time, expirationTimestamp, mf2, o2ver)

        def create(matcher: PrivateKeyAccount = sender1,
                   buyOrder: Order = buy,
                   sellOrder: Order = sell,
                   amount: Long = buyAmount,
                   price: Long = sellPrice,
                   buyMatcherFee: Long = mf1,
                   sellMatcherFee: Long = 1,
                   fee: Long = 1,
                   timestamp: Long = expirationTimestamp - Order.MaxLiveTime) = {
          ExchangeTransactionV2.create(
            matcher = sender1,
            buyOrder = buyOrder,
            sellOrder = sellOrder,
            amount = amount,
            price = price,
            buyMatcherFee = buyMatcherFee,
            sellMatcherFee = sellMatcherFee,
            fee = fee,
            timestamp = timestamp
          )
        }

        buy.version shouldBe o1ver
        sell.version shouldBe o2ver

        create() shouldBe an[Right[_, _]]
        create(fee = pow(10, 18).toLong) shouldBe an[Right[_, _]]
        create(amount = Order.MaxAmount) shouldBe an[Right[_, _]]

        create(amount = -1) shouldBe an[Left[_, _]]
        create(amount = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(price = -1) shouldBe an[Left[_, _]]
        create(sellMatcherFee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(buyMatcherFee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(fee = Order.MaxAmount + 1) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateMatcher(sender2)) shouldBe an[Left[_, _]]
        create(sellOrder = buy.updateMatcher(sender2)) shouldBe an[Left[_, _]]
        create(
          buyOrder = buy.updatePair(buy.assetPair.copy(amountAsset = None)),
          sellOrder = sell.updatePair(sell.assetPair.copy(priceAsset = Some(ByteStr(Array(1: Byte)))))
        ) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateExpiration(1L)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updateAmount(-1)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateAmount(-1)) shouldBe an[Left[_, _]]

        create(price = buy.price + 1) shouldBe an[Left[_, _]]
        create(price = sell.price - 1) shouldBe an[Left[_, _]]

        create(buyOrder = buy.updateType(OrderType.SELL)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateAmount(0)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateAmount(-1)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateAmount(Order.MaxAmount + 1)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updatePair(buy.assetPair.copy(amountAsset = sell.assetPair.priceAsset))) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateExpiration(1L)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateExpiration(buy.expiration + 1)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updatePrice(-1)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updatePrice(price = sellPrice - 1)) shouldBe an[Left[_, _]]
        create(buyOrder = buy.updateMatcher(sender2)) shouldBe an[Left[_, _]]

        create(sellOrder = sell.updateType(OrderType.BUY)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updateAmount(0)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updateAmount(-1)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updateAmount(Order.MaxAmount + 1)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updatePair(sell.assetPair.copy(priceAsset = buy.assetPair.amountAsset))) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updateExpiration(1L)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updateExpiration(sell.expiration + 1)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updatePrice(-1)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updatePrice(price = buyPrice + 1)) shouldBe an[Left[_, _]]
        create(sellOrder = sell.updateMatcher(sender2)) shouldBe an[Left[_, _]]

        create(
          buyOrder = buy.updatePair(buy.assetPair.copy(amountAsset = None)),
          sellOrder = sell.updatePair(sell.assetPair.copy(priceAsset = Some(ByteStr(Array(1: Byte)))))
        ) shouldBe an[Left[_, _]]
    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: PrivateKeyAccount): Either[ValidationError, ExchangeTransaction] = {
    val mf     = 300000L
    val amount = math.min(buy.amount, sell.amount)
    ExchangeTransactionV2.create(
      matcher = matcher,
      buyOrder = buy,
      sellOrder = sell,
      amount = amount,
      price = price,
      buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
      sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
      fee = mf,
      timestamp = ntpTime.correctedTime()
    )
  }

  property("Test transaction with small amount and expired order") {
    forAll(
      accountGen,
      accountGen,
      accountGen,
      assetPairGen,
      Gen.oneOf((1: Byte, 1: Byte), (1: Byte, 2: Byte), (2: Byte, 1: Byte), (2: Byte, 2: Byte))
    ) { (sender1: PrivateKeyAccount, sender2: PrivateKeyAccount, matcher: PrivateKeyAccount, pair: AssetPair, versions) =>
      val time                = ntpTime.correctedTime()
      val expirationTimestamp = time + Order.MaxLiveTime
      val buyPrice            = 1 * Order.PriceConstant
      val sellPrice           = (0.50 * Order.PriceConstant).toLong
      val mf                  = 300000L
      val (o1ver, o2ver)      = versions

      val sell = Order.sell(sender2, matcher, pair, 2, sellPrice, time, expirationTimestamp, mf, o1ver)
      val buy  = Order.buy(sender1, matcher, pair, 1, buyPrice, time, expirationTimestamp, mf, o2ver)

      createExTx(buy, sell, sellPrice, matcher) shouldBe an[Right[_, _]]

      val sell1 = Order.sell(sender1, matcher, pair, 1, buyPrice, time, time - 1, mf, o1ver)
      createExTx(buy, sell1, buyPrice, matcher) shouldBe Left(OrderValidationError(sell1, "expiration should be > currentTime"))
    }
  }

  property("JSON format validation V2") {
    val acc1           = recipientAccount
    val acc2           = accountGen.sample.get
    val matcherAccount = senderAccount

    val buy = OrderV2(
      PublicKeyAccount(acc1.publicKey),
      PublicKeyAccount(matcherAccount.publicKey),
      AssetPair.createAssetPair(AssetPair.WestAsset, "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.BUY,
      2,
      6000000000L,
      1526992336241L,
      1529584336241L,
      1,
      Proofs(Seq(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get))
    )

    val sell = OrderV1(
      PublicKeyAccount(acc2.publicKey),
      PublicKeyAccount(matcherAccount.publicKey),
      AssetPair.createAssetPair(AssetPair.WestAsset, "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      3,
      5000000000L,
      1526992336241L,
      1529584336241L,
      2,
      Base58.decode("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get
    )

    val tx = ExchangeTransactionV2
      .create(
        buy,
        sell,
        2,
        5000000000L,
        1,
        1,
        1,
        1526992336241L,
        Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))
      )
      .explicitGet()

    val js = Json.parse(s"""{
      |  "type": 7,
      |  "id": "${tx.id()}",
      |  "sender": "${senderAccount.address}",
      |  "senderPublicKey": "$senderPkBase58",
      |  "fee": 1,
      |  "timestamp": 1526992336241,
      |  "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
      |  "version": 2,
      |  "order1": {
      |    "version": 2,
      |    "id": "${buy.id()}",
      |    "sender": "${acc1.address}",
      |    "senderPublicKey": "${acc1.publicKeyBase58}",
      |    "matcherPublicKey": "$senderPkBase58",
      |    "assetPair": {
      |      "amountAsset": null,
      |      "priceAsset": "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"
      |    },
      |    "orderType": "buy",
      |    "price": 6000000000,
      |    "amount": 2,
      |    "timestamp": 1526992336241,
      |    "expiration": 1529584336241,
      |    "matcherFee": 1,
      |    "signature": "2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs",
      |    "proofs":["2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs"]
      |  },
      |  "order2": {
      |    "version": 1,
      |    "id": "${sell.id()}",
      |    "sender": "${acc2.address}",
      |    "senderPublicKey": "${acc2.publicKeyBase58}",
      |    "matcherPublicKey": "$senderPkBase58",
      |    "assetPair": {
      |      "amountAsset": null,
      |      "priceAsset": "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"
      |    },
      |    "orderType": "sell",
      |    "price": 5000000000,
      |    "amount": 3,
      |    "timestamp": 1526992336241,
      |    "expiration": 1529584336241,
      |    "matcherFee": 2,
      |    "signature": "2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq",
      |    "proofs": ["2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq"]
      |  },
      |  "price": 5000000000,
      |  "amount": 2,
      |  "buyMatcherFee": 1,
      |  "sellMatcherFee": 1
      |}""".stripMargin)

    js shouldEqual tx.json()
  }
}
