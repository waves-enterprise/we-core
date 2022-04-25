package com.wavesenterprise.lang

import com.wavesenterprise.lang.Common._
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.v1.compiler.CompilerV1
import com.wavesenterprise.lang.v1.compiler.Terms._
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesenterprise.lang.v1.parser.Expressions
import com.wavesenterprise.lang.v1.testing.ScriptGen
import com.wavesenterprise.lang.v1.{FunctionHeader, Serde}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Assertion, FreeSpec, Matchers}
import scodec.bits.ByteVector

class SerdeTest extends FreeSpec with ScalaCheckPropertyChecks with Matchers with ScriptGen with NoShrink {

  "roundtrip" - {
    "CONST_LONG" in roundTripTest(CONST_LONG(1))
    "CONST_BYTEVECTOR" in roundTripTest(CONST_BYTEVECTOR(ByteVector[Byte](1)))
    "CONST_STRING" in roundTripTest(CONST_STRING("foo"))

    "IF" in roundTripTest(IF(TRUE, CONST_LONG(0), CONST_LONG(1)))

    "BLOCK" in roundTripTest(
      BLOCK(
        let = LET("foo", TRUE),
        body = FALSE
      )
    )

    "REF" in roundTripTest(REF("foo"))
    "TRUE" in roundTripTest(TRUE)
    "FALSE" in roundTripTest(FALSE)

    "GETTER" in roundTripTest(GETTER(REF("foo"), "bar"))

    "FUNCTION_CALL" - {
      "native" in roundTripTest(
        FUNCTION_CALL(
          function = FunctionHeader.Native(1),
          args = List(TRUE)
        )
      )

      "user" in roundTripTest(
        FUNCTION_CALL(
          function = FunctionHeader.User("foo"),
          args = List(TRUE)
        )
      )

      "empty args" in roundTripTest(
        FUNCTION_CALL(
          function = FunctionHeader.User("foo"),
          args = List.empty
        )
      )
    }

    "general" in forAll(BOOLgen(10)) {
      case (untypedExpr, _) => roundTripTest(untypedExpr)
    }

    "stack safety" in {
      val bigSum = (1 to 10000).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
        FUNCTION_CALL(
          function = PureContext.sumLong,
          args = List(r, CONST_LONG(i))
        )
      }

      val expr: EXPR = FUNCTION_CALL(
        function = PureContext.eq,
        args = List(CONST_LONG(1), bigSum)
      )

      Serde.serialize(expr).nonEmpty shouldBe true
    }
  }

  "spec input" in {
    val byteArr   = Array[Byte](1, 113, -1, 63, 0, -1, 127, 0, -1, 39, -1, 87, -41, 50, -111, -38, 12, 1, 0, -19, 101, -128, -1, 54)
    val (r, time) = measureTime(Serde.deserialize(byteArr))

    r shouldBe an[Either[_, _]]
    time should be <= 1000L
  }

  "any input" in {
    forAll(Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary)) { byteArr =>
      val (r, time) = measureTime(Serde.deserialize(byteArr))

      r shouldBe an[Either[_, _]]
      time should be <= 1000L
    }
  }

  def measureTime[A](f: => A): (A, Long) = {
    val start  = System.currentTimeMillis()
    val result = f
    (result, System.currentTimeMillis() - start)
  }

  private def roundTripTest(untypedExpr: Expressions.EXPR): Assertion = {
    val typedExpr = CompilerV1(PureContext.build(V1).compilerContext, untypedExpr).map(_._1).explicitGet()
    roundTripTest(typedExpr)
  }

  private def roundTripTest(typedExpr: EXPR): Assertion = {
    val encoded = Serde.serialize(typedExpr)
    encoded.nonEmpty shouldBe true

    val decoded = Serde.deserialize(encoded).explicitGet()
    withClue(s"encoded bytes: [${encoded.mkString(", ")}]") {
      decoded shouldEqual typedExpr
    }
  }
}
