package com.wavesenterprise.settings

import cats.implicits.{catsStdInstancesForEither, catsSyntaxFlatMapOps}
import com.wavesenterprise.settings.Constants.{UnitsInWest, WestDecimals}
import com.wavesenterprise.transaction.ValidationError
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.utils.NumberUtils.ValidBigDecimal
import play.api.libs.json._

case class WestAmount(units: Long) extends AnyVal {
  override def toString: String = units.toString
}

object WestAmount {
  private val tokenName      = "WEST"
  private val minDecimalUnit = BigDecimal(1) / Constants.UnitsInWest

  /**
    * Maximum possible sum in WEST is limited by 10 billion
    * There are actually 1 billion real WEST tokens in the Mainnet, part of which are already burned
    */
  private val maxAmountInWest = 10e9.toLong

  implicit val WestAmountFormat: Format[WestAmount] = new Format[WestAmount] {
    override def writes(o: WestAmount): JsValue = JsNumber(o.units)
    override def reads(json: JsValue): JsResult[WestAmount] = json match {
      case JsNumber(v) => JsSuccess(WestAmount(v.toLongExact))
      case _           => JsError("Expected JsNumber")
    }
  }

  def fromString(input: String): Either[ValidationError, WestAmount] = {
    input.trim.split(' ') match {
      case Array(ValidBigDecimal(decimal), `tokenName`) =>
        checkAmount(decimal, isExpectedEven = false)
          .map(_ => WestAmount((decimal * Constants.UnitsInWest).toLongExact))
      case Array(ValidBigDecimal(units)) if units >= 0 && units % 1 == 0 =>
        checkAmount(units, isExpectedEven = true)
          .map(_ => WestAmount(units.toLongExact))
      case _ =>
        Left(ValidationError.GenericError(s"Failed to parse '$input' as WEST Amount"))
    }
  }

  def checkAmount(amount: BigDecimal, isExpectedEven: Boolean): Either[ValidationError, Unit] = {
    Either.cond(amount >= 0, (), GenericError("WEST amount should be positive")) >>
      (if (isExpectedEven) {
         Either.cond(amount <= maxAmountInWest * UnitsInWest, (), GenericError("WEST amounts greater than 10 billion are not allowed")) >>
           Either.cond(!isExpectedEven || (isExpectedEven && amount % 1 == 0), (), GenericError("Expected an even number"))
       } else {
         Either.cond(amount <= maxAmountInWest, (), GenericError("WEST amounts greater than 10 billion are not allowed")) >>
           Either.cond(amount % 1 >= minDecimalUnit || amount % 1 == 0, (), GenericError(s"Only $WestDecimals decimal units are allowed in WEST"))
       })
  }

}
