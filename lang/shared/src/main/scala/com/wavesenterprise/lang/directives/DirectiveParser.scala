package com.wavesenterprise.lang.directives

import fastparse.MultiLineWhitespace._
import fastparse.Parsed.Success
import fastparse._

object DirectiveParser {

  val start = "{-#"
  val end   = "#-}"

  private def space[_: P]: P[Unit] =
    P(CharIn(" ", "\t", "\r", "\n").rep)

  private def directiveKeyP[_: P]: P[String] =
    P(CharIn("a-zA-Z0-9_")).repX(1).!

  private def directiveValueP[_: P]: P[String] =
    P(CharIn("a-zA-Z0-9/\\.,")).repX(1).!

  private def parser[_: P]: P[Option[Directive]] =
    P(space ~ start ~ directiveKeyP ~ directiveValueP ~ end ~ space)
      .map {
        case (keyRaw, valueRaw) =>
          DirectiveKey.dictionary
            .get(keyRaw)
            .map { key =>
              Directive(key, valueRaw)
            }
      }

  def apply(input: String): List[Directive] = {
    input
      .split("\n")
      .map(parse(_, parser(_)))
      .collect {
        case Success(Some(value), _) => value
      }
      .toList
  }
}
