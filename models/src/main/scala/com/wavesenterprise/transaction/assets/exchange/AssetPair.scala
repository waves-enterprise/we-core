package com.wavesenterprise.transaction.assets.exchange

import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction._
import com.wavesenterprise.transaction.assets.exchange.Order.assetIdBytes
import com.wavesenterprise.transaction.assets.exchange.Validation.booleanOperators
import play.api.libs.json.{JsObject, Json}

import scala.util.{Success, Try}

case class AssetPair(amountAsset: Option[AssetId], priceAsset: Option[AssetId]) {
  import AssetPair._

  lazy val priceAssetStr: String = assetIdStr(priceAsset)

  lazy val amountAssetStr: String = assetIdStr(amountAsset)
  override def toString: String   = key
  def key: String                 = amountAssetStr + "-" + priceAssetStr
  def isValid: Validation         = (amountAsset != priceAsset) :| "Invalid AssetPair"
  def bytes: Array[Byte]          = assetIdBytes(amountAsset) ++ assetIdBytes(priceAsset)
  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.map(_.base58),
    "priceAsset"  -> priceAsset.map(_.base58)
  )
  def reverse = AssetPair(priceAsset, amountAsset)

  def assets: Set[Option[AssetId]] = Set(amountAsset, priceAsset)
}

object AssetPair {
  val WestAsset = "WEST"

  def assetIdStr(aid: Option[AssetId]): String = aid.fold(WestAsset)(_.base58)

  def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case WestAsset => Success(None)
    case other     => ByteStr.decodeBase58(other).map(Option(_))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)
}
