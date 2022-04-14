package com.wavesenterprise.transaction.smart.script

import com.wavesenterprise.lang.v1.FunctionHeader
import com.wavesenterprise.lang.v1.compiler.Terms._
import com.wavesenterprise.lang.v1.evaluator.FunctionIds._
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesenterprise.lang.v1.testing.TypedScriptGen
import com.wavesenterprise.state.diffs.ProduceError.produce
import com.wavesenterprise.transaction.smart.script.v1.ScriptV1
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scodec.bits.ByteVector

class ScriptV1Test extends PropSpec with ScalaCheckPropertyChecks with Matchers with TypedScriptGen {

  property("ScriptV1.apply should permit BOOLEAN scripts") {
    forAll(BOOLEANgen(10)) { expr =>
      ScriptV1(expr) shouldBe 'right
    }
  }

  property("ScriptV1.apply should deny too complex scripts") {
    val byteVector = CONST_BYTEVECTOR(ByteVector(1))
    val expr = (1 to 21)
      .map { _ =>
        FUNCTION_CALL(
          function = FunctionHeader.Native(SIGVERIFY),
          args = List(byteVector, byteVector, byteVector)
        )
      }
      .reduceLeft[EXPR](IF(_, _, FALSE))

    ScriptV1(expr) should produce("Script is too complex")
  }

  property("ScriptV1.apply should deny too big scripts") {
    val bigSum = (1 to 100).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
      FUNCTION_CALL(
        function = FunctionHeader.Native(SUM_LONG),
        args = List(r, CONST_LONG(i))
      )
    }
    val expr = (1 to 9).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
      FUNCTION_CALL(
        function = PureContext.eq.header,
        args = List(r, bigSum)
      )
    }

    ScriptV1(expr) should produce("Script is too large")
  }

  property("19 sigVerify should fit in maxSizeInBytes") {
    val byteVector = CONST_BYTEVECTOR(ByteVector(1))
    val expr = (1 to 19)
      .map { _ =>
        FUNCTION_CALL(
          function = FunctionHeader.Native(SIGVERIFY),
          args = List(byteVector, byteVector, byteVector)
        )
      }
      .reduceLeft[EXPR](IF(_, _, FALSE))

    ScriptV1(expr) shouldBe 'right
  }

}
