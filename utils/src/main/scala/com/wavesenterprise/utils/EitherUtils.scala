package com.wavesenterprise.utils

object EitherUtils {
  implicit class EitherExt[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value)  => throw new Exception(value.toString)
      case Right(value) => value
    }
  }
}
