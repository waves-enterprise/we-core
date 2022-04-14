package com.wavesenterprise.lang

import com.wavesenterprise.lang.directives.DirectiveKey.LANGUAGE_VERSION
import com.wavesenterprise.lang.directives.{Directive, DirectiveParser}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DirectiveParserTest extends PropSpec with ScalaCheckPropertyChecks with Matchers {

  def parse(s: String): List[Directive] = DirectiveParser(s)

  property("parse LANGUAGE_VERSION directive") {
    parse("{-# LANGUAGE_VERSION 10 #-}") shouldBe List(Directive(LANGUAGE_VERSION, "10"))
    parse("""
        |
        |{-# LANGUAGE_VERSION 10 #-}
        |
      """.stripMargin) shouldBe List(Directive(LANGUAGE_VERSION, "10"))
  }
}
