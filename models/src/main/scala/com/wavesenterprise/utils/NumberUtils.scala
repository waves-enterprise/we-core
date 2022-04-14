package com.wavesenterprise.utils

import com.wavesenterprise.settings.Constants.UnitsInWest

import scala.util.Try

object NumberUtils {
  implicit class DoubleExt(val d: Double) extends AnyVal {
    def west: Long = (d * UnitsInWest).toLong
  }

  object ValidBigDecimal {
    def unapply(arg: String): Option[BigDecimal] = {
      Try(BigDecimal(arg)).toOption
    }
  }
}
