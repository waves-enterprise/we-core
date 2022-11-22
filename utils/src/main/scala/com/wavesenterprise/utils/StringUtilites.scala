package com.wavesenterprise.utils

import cats.Show

object StringUtilites {

  def dashes(s: String): String =
    s.replace("\n", "\n--")

  implicit def betterOptionShow[A: Show]: Show[Option[A]] =
    Show.show[Option[A]] {
      case Some(value) => implicitly[Show[A]].show(value)
      case None        => "None"
    }

  object ValidateAsciiAndRussian {
    def findNotValid(s: String): Option[String] = {
      //finding for for all symbols except ascii non-control and  russian letters
      val pattern = raw"[^\x21-\x7Eа-яА-Я]*".r
      val res = pattern
        .findAllMatchIn(s)
        .filterNot(_.matched.isBlank())
        .map(_.group(0))
        .toSeq
        .flatten
        .toSet
        .mkString
      if (res.isEmpty) None else Some(res)
    }
    val keyAndForbiddenSymbols: String => Option[String] =
      s => findNotValid(s).map(notValidChars => s"$s -> $notValidChars")

    val keyToForbiddenSymbols: String => Option[Map[String, String]] =
      s => findNotValid(s).map(notValidChars => Map(s -> notValidChars))

    def notValidOrRight(list: List[String]): Either[String, Unit] = {
      val findingErrors = list.map(keyAndForbiddenSymbols).flatten.mkString("; ")
      if (findingErrors.isBlank) Right(())
      else Left(findingErrors)
    }
    def notValidMapOrRight(list: List[String]): Either[Map[String, String], Unit] = {
      val findingErrors = list
        .map(keyToForbiddenSymbols)
        .flatten
        .foldRight(Map[String, String]()) { (m, acc) =>
          acc ++ m
        }
      if (findingErrors.isEmpty) Right(())
      else Left(findingErrors)
    }

    def notValidOrRight(s: String): Either[String, Unit]                 = notValidOrRight(List(s))
    def notValidMapOrRight(s: String): Either[Map[String, String], Unit] = notValidMapOrRight(List(s))

    val mapToString: Map[String, String] => String =
      _.map(_.productIterator.mkString(" -> "))
        .mkString("; ")

    val stringToMap: String => Map[String, String] =
      _.split("; ").map(_.split(" -> ")).map { case Array(k, v) => (k, v) }.toMap

  }

}
