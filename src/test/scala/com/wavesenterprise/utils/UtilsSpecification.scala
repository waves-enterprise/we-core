package com.wavesenterprise.utils

import com.wavesenterprise.lang.v1.compiler.Terms.{FUNCTION_CALL, TRUE}
import com.wavesenterprise.lang.v1.compiler.Types.BOOLEAN
import com.wavesenterprise.lang.v1.evaluator.ctx.{EvaluationContext, UserFunction}
import com.wavesenterprise.utils.SmartContractV1Utils.estimate
import com.wavesenterprise.utils.StringUtilites.ValidateAsciiAndRussian.notValidOrRight
import org.scalatest.{FreeSpec, Matchers}

class UtilsSpecification extends FreeSpec with Matchers {

  "estimate()" - {
    "handles functions that depend on each other" in {
      val callee = UserFunction("callee", BOOLEAN, "test users true")(TRUE)
      val caller = UserFunction("caller", BOOLEAN, "test call")(FUNCTION_CALL(callee.header, List.empty))
      val ctx = EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Seq(caller, callee).map(f => f.header -> f)(collection.breakOut)
      )
      estimate(ctx).size shouldBe 2
    }
  }
  "validate ascii and russian letter" - {
    "pass string" in {
      notValidOrRight("abc123Россия") shouldBe Right(())
    }
    "pass list string" in {
      notValidOrRight(List("abc123Россия", "good")) shouldBe Right(())
    }
    "give key with forbidden symbols string" in {
      notValidOrRight("abc123Росåß∂ƒсия") shouldBe Left("abc123Росåß∂ƒсия -> åß∂ƒ")
    }
    "give keys with forbidden symbols string" in {
      notValidOrRight(List("abc123Россия", "åß∂ƒabc123Россия", "Ω≈ç√")) shouldBe Left("åß∂ƒabc123Россия -> åß∂ƒ; Ω≈ç√ -> Ω≈ç√")
    }
  }
}
