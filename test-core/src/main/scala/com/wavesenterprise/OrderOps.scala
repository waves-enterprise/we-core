package com.wavesenterprise

import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.transaction.Proofs
import com.wavesenterprise.transaction.assets.exchange._

class OrderOps(val o: Order) extends AnyVal {
  @inline def copy(withV1: OrderV1 => OrderV1, withV2: OrderV2 => OrderV2): Order = {
    o match {
      case o1 @ OrderV1(_, _, _, _, _, _, _, _, _, _) => withV1(o1)
      case o2 @ OrderV2(_, _, _, _, _, _, _, _, _, _) => withV2(o2)
    }
  }

  @inline def updateProofs(p: Proofs): Order = {
    copy(
      _.copy(proofs = p),
      _.copy(proofs = p)
    )
  }

  @inline def updateExpiration(expiration: Long): Order = {
    copy(
      _.copy(expiration = expiration),
      _.copy(expiration = expiration)
    )
  }
  @inline def updateTimestamp(timestamp: Long): Order = {
    copy(
      _.copy(timestamp = timestamp),
      _.copy(timestamp = timestamp)
    )
  }
  @inline def updateFee(fee: Long): Order = {
    copy(
      _.copy(matcherFee = fee),
      _.copy(matcherFee = fee)
    )
  }
  @inline def updateAmount(amount: Long): Order = {
    copy(
      _.copy(amount = amount),
      _.copy(amount = amount)
    )
  }
  @inline def updatePrice(price: Long): Order = {
    copy(
      _.copy(price = price),
      _.copy(price = price)
    )
  }
  @inline def updateMatcher(pk: PrivateKeyAccount): Order = {
    copy(
      _.copy(matcherPublicKey = pk),
      _.copy(matcherPublicKey = pk)
    )
  }
  @inline def updateSender(pk: PrivateKeyAccount): Order = {
    copy(
      _.copy(senderPublicKey = pk),
      _.copy(senderPublicKey = pk)
    )
  }
  @inline def updatePair(pair: AssetPair): Order = {
    copy(
      _.copy(assetPair = pair),
      _.copy(assetPair = pair)
    )
  }
  @inline def updateType(t: OrderType): Order = {
    copy(
      _.copy(orderType = t),
      _.copy(orderType = t)
    )
  }
}

object OrderOps {
  implicit def toOps(o: Order): OrderOps = new OrderOps(o)
}
