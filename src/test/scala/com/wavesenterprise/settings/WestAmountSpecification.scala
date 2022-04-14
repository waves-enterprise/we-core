package com.wavesenterprise.settings

import org.scalatest.{FlatSpec, Matchers}

class WestAmountSpecification extends FlatSpec with Matchers {

  "WestAmount" should "return empty when value is not number" in {
    WestAmount.fromString("foo") should be('left)
  }

  it should "correctly parse 0.1 WEST" in {
    WestAmount.fromString("0.1 WEST").map(_.units) should be(Right(10000000L))
  }

  it should "correctly parse 0.00000012 WEST" in {
    WestAmount.fromString("0.00000012 WEST").map(_.units) should be(Right(12L))
  }

  it should "correctly parse 0.000014 WEST" in {
    WestAmount.fromString("0.000014 WEST").map(_.units) should be(Right(1400L))
  }

  it should "correctly parse 0.00001400 WEST" in {
    WestAmount.fromString("0.000014 WEST").map(_.units) should be(Right(1400L))
  }

  it should "correctly parse 2 WEST" in {
    WestAmount.fromString("2 WEST").map(_.units) should be(Right(200000000L))
  }

  it should "correctly parse 0 WEST" in {
    WestAmount.fromString("0 WEST").map(_.units) should be(Right(0))
  }

  it should "return Left when decimal value is not positive" in {
    WestAmount.fromString("-0.001 WEST") should be('left)
  }

  it should "return Left when decimal value is less than 1 token" in {
    WestAmount.fromString("0.000000001 WEST") should be('left)
  }

  it should "return Left when decimal value contains part which less than 1 token" in {
    WestAmount.fromString("14.000000001 WEST") should be('left)
  }

  it should "correctly parse 14 units" in {
    WestAmount.fromString("14").map(_.units) should be(Right(14L))
  }

  it should "correctly parse 0 units" in {
    WestAmount.fromString("0").map(_.units) should be(Right(0L))
  }

  it should "return Left when units value is not positive" in {
    WestAmount.fromString("-2") should be('left)
  }

  it should "return Left when units value is less than 1 token" in {
    WestAmount.fromString("0.1") should be('left)
  }

  it should "return Left when units value contains part which less than 1 token" in {
    WestAmount.fromString("5.1") should be('left)
  }

  it should "return Left on amounts bigger than 10 billion tokens" in {
    val tooMuch = (10e9 + 1).toLong
    WestAmount.fromString(s"$tooMuch WEST") should be('left)
    WestAmount.fromString((tooMuch * Constants.UnitsInWest).toString) should be('left)
  }
}
