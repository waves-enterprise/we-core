package com.wavesenterprise.lang

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesenterprise.lang.Common._
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.Testing._
import com.wavesenterprise.lang.v1.CTX
import com.wavesenterprise.lang.v1.compiler.Terms._
import com.wavesenterprise.lang.v1.compiler.Types.{FINAL, LONG}
import com.wavesenterprise.lang.v1.compiler.{CompilerV1, Terms}
import com.wavesenterprise.lang.v1.evaluator.EvaluatorV1
import com.wavesenterprise.lang.v1.evaluator.ctx._
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.{PureContext, _}
import com.wavesenterprise.lang.v1.parser.Parser
import com.wavesenterprise.lang.v1.testing.ScriptGen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class IntegrationTest extends PropSpec with ScalaCheckPropertyChecks with ScriptGen with Matchers with NoShrink {

  property("simple let") {
    val src =
      """
        |let a = 1
        |let b = a + a
        |b + b + b
      """.stripMargin
    eval[EVALUATED](src) shouldBe evaluated(6)
  }

  property("proper error message") {
    val src =
      """
        |match p {
        |  case pa: PointA => let x = 3
        |  case _ => throw()
        |}
      """.stripMargin
    eval[EVALUATED](src) should produce("can't parse the expression")
  }

  property("patternMatching") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointB => 1
        |  case pa: PointC => 2
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("patternMatching with named union types") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointBC => 1
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("union types have fields") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.YB
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(3)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(41)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(42)
  }

  property("union types have  only common fields") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.X
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointCInstance)) should produce("Compilation failed: Undefined field `X`")
  }

  property("patternMatching _") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case _: PointB  => 1
         |  case _: PointC => 2
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("patternMatching any type") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  property("patternMatching block") {
    val sampleScript =
      """|
         |match (let x = 1; p) {
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(1)
  }

  private def eval[T <: EVALUATED](code: String, pointInstance: Option[CaseObj] = None, pointType: FINAL = AorBorC): Either[String, T] = {
    val untyped                                                = Parser(code).get.value
    val lazyVal                                                = LazyVal(EitherT.pure(pointInstance.orNull))
    val stringToTuple: Map[String, ((FINAL, String), LazyVal)] = Map(("p", ((pointType, "Test variable"), lazyVal)))
    val ctx: CTX =
      Monoid.combine(PureContext.build(V1), CTX(sampleTypes, stringToTuple, Array.empty))
    val typed = CompilerV1(ctx.compilerContext, untyped)
    typed.flatMap(v => EvaluatorV1[T](ctx.evaluationContext, v._1))
  }

  property("function call") {
    eval[EVALUATED]("10 + 2") shouldBe evaluated(12)
  }

  property("max values and operation order") {
    val longMax = Long.MaxValue
    val longMin = Long.MinValue
    eval(s"$longMax + 1 - 1") shouldBe Left("long overflow")
    eval(s"$longMin - 1 + 1") shouldBe Left("long overflow")
    eval(s"$longMax - 1 + 1") shouldBe evaluated(longMax)
    eval(s"$longMin + 1 - 1") shouldBe evaluated(longMin)
    eval(s"$longMax / $longMin + 1") shouldBe evaluated(0)
    eval(s"($longMax / 2) * 2") shouldBe evaluated(longMax - 1)
    eval[EVALUATED]("fraction(9223372036854775807, 3, 2)") shouldBe Left(
      s"Long overflow: value `${BigInt(Long.MaxValue) * 3 / 2}` greater than 2^63-1")
    eval[EVALUATED]("fraction(-9223372036854775807, 3, 2)") shouldBe Left(
      s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1")
    eval[EVALUATED](s"$longMax + fraction(-9223372036854775807, 3, 2)") shouldBe Left(
      s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1")
    eval[EVALUATED](s"2 + 2 * 2") shouldBe evaluated(6)
    eval("2 * 3 == 2 + 4") shouldBe evaluated(true)
  }

  property("equals works on primitive types") {
    eval[EVALUATED]("base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8' == base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8'") shouldBe evaluated(true)
    eval[EVALUATED]("1 == 2") shouldBe evaluated(false)
    eval[EVALUATED]("3 == 3") shouldBe evaluated(true)
    eval[EVALUATED]("false == false") shouldBe evaluated(true)
    eval[EVALUATED]("true == false") shouldBe evaluated(false)
    eval[EVALUATED]("true == true") shouldBe evaluated(true)
    eval[EVALUATED]("""   "x" == "x"     """) shouldBe evaluated(true)
    eval[EVALUATED]("""   "x" == "y"     """) shouldBe evaluated(false)
    eval[EVALUATED]("""   "x" != "y"     """) shouldBe evaluated(true)
    eval[EVALUATED]("""   "x" == 3     """) should produce("Can't match inferred types")
    eval[EVALUATED]("""   "x" != 3     """) should produce("Can't match inferred types")
    eval[EVALUATED](""" let union = if(true) then "x" else 3; union == "x"   """) shouldBe evaluated(true)
  }

  property("equals some lang structure") {
    eval[EVALUATED]("let x = (-7763390488025868909>-1171895536391400041); let v = false; (v&&true)") shouldBe evaluated(false)
    eval[EVALUATED]("let mshUmcl = (if(true) then true else true); true || mshUmcl") shouldBe evaluated(true)
    eval[EVALUATED]("""if(((1+-1)==-1)) then 1 else (1+1)""") shouldBe evaluated(2)
    eval[EVALUATED]("""((((if(true) then 1 else 1)==2)||((if(true)
        |then true else true)&&(true||true)))||(if(((1>1)||(-1>=-1)))
        |then (-1>=1) else false))""".stripMargin) shouldBe evaluated(true)
  }

  property("sum/mul/div/mod/fraction functions") {
    eval[EVALUATED]("(10 + 10)#jhk\n ") shouldBe evaluated(20)
    eval[EVALUATED]("(10 * 10)") shouldBe evaluated(100)
    eval[EVALUATED]("(10 / 3)") shouldBe evaluated(3)
    eval[EVALUATED]("(10 % 3)") shouldBe evaluated(1)
    eval[EVALUATED]("fraction(9223372036854775807, -2, -4)") shouldBe evaluated(Long.MaxValue / 2)
  }

  def compile(script: String): Either[String, Terms.EXPR] = {
    val compiler = new CompilerV1(CTX.empty.compilerContext)
    compiler.compile(script, List.empty)
  }

  property("wrong script return type") {
    compile("1") should produce("should return boolean")
    compile(""" "string" """) should produce("should return boolean")
    compile(""" base58'string' """) should produce("should return boolean")
  }

  property("equals works on elements from Gens") {
    List(CONST_LONGgen, SUMgen(50), INTGen(50)).foreach(gen =>
      forAll(for {
        (expr, res) <- gen
        str         <- toString(expr)
      } yield (str, res)) {
        case (str, res) =>
          withClue(str) {
            eval[EVALUATED](str) shouldBe evaluated(res)
          }
    })

    forAll(for {
      (expr, res) <- BOOLgen(50)
      str         <- toString(expr)
    } yield (str, res)) {
      case (str, res) =>
        withClue(str) {
          eval[EVALUATED](str) shouldBe evaluated(res)
        }
    }
  }

  property("Match with not case types") {
    eval[EVALUATED]("""
        |
        |let a = if (true) then 1 else ""
        |
        |match a {
        | case x: Int => x 
        | case y: String => 2
        |}""".stripMargin) shouldBe evaluated(1)
  }

  property("allow unions in pattern matching") {
    val sampleScript =
      """match p {
        |  case p1: PointBC => {
        |    match p1 {
        |      case pb: PointB => pb.X
        |      case pc: PointC => pc.YB
        |    }
        |  }
        |  case other => throw()
        |}""".stripMargin
    eval[EVALUATED](sampleScript, Some(pointBInstance)) shouldBe evaluated(3)
    eval[EVALUATED](sampleScript, Some(pointCInstance)) shouldBe evaluated(42)
  }

  property("different types, same name of field") {
    val sampleScript =
      """match (p.YB) {
        | case l: Int => l
        | case u: Unit => 1
        | }
      """.stripMargin
    eval[EVALUATED](sampleScript, Some(pointCInstance), CorD) shouldBe evaluated(42)
    eval[EVALUATED](sampleScript, Some(pointDInstance1), CorD) shouldBe evaluated(43)
    eval[EVALUATED](sampleScript, Some(pointDInstance2), CorD) shouldBe evaluated(1)

    eval[EVALUATED]("p.YB", Some(pointCInstance), CorD) shouldBe evaluated(42)
    eval[EVALUATED]("p.YB", Some(pointDInstance1), CorD) shouldBe evaluated(43)
    eval[EVALUATED]("p.YB", Some(pointDInstance2), CorD) shouldBe evaluated(unit)
  }

  property("throw") {
    val script =
      """
        |let result = match p {
        |  case a: PointA => 0
        |  case b: PointB => throw()
        |  case c: PointC => throw("arrgh")
        |}
        |result
      """.stripMargin
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(0)
    eval[EVALUATED](script, Some(pointBInstance)) shouldBe Left("Explicit script termination")
    eval[EVALUATED](script, Some(pointCInstance)) shouldBe Left("arrgh")
  }

  property("context won't change after inner let") {
    val script = "{ let x = 3; x } + { let x = 5; x}"
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(8)
  }

  property("contexts of different if parts do not affect each other") {
    val script = "if ({let x= 0; x > 0 }) then { let x = 3; x } else { let x = 5; x}"
    eval[EVALUATED](script, Some(pointAInstance)) shouldBe evaluated(5)
  }

  property("context won't change after execution of a user function") {
    val doubleFst = UserFunction("ID", LONG, "D", ("x", LONG, "X")) {
      FUNCTION_CALL(PureContext.sumLong.header, List(REF("x"), REF("x")))
    }

    val context = Monoid.combine(
      PureContext.build(V1).evaluationContext,
      EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map("x"                -> LazyVal(EitherT.pure(CONST_LONG(3l)))),
        functions = Map(doubleFst.header -> doubleFst)
      )
    )

    val expr = FUNCTION_CALL(PureContext.sumLong.header, List(FUNCTION_CALL(doubleFst.header, List(CONST_LONG(1000l))), REF("x")))
    ev[CONST_LONG](context, expr) shouldBe evaluated(2003l)
  }

  property("context won't change after execution of an inner block") {
    val context = Monoid.combine(
      PureContext.build(V1).evaluationContext,
      EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map("x" -> LazyVal(EitherT.pure(CONST_LONG(3l)))),
        functions = Map.empty
      )
    )

    val expr = FUNCTION_CALL(
      function = PureContext.sumLong.header,
      args = List(
        BLOCK(
          let = LET("x", CONST_LONG(5l)),
          body = REF("x")
        ),
        REF("x")
      )
    )
    ev[CONST_LONG](context, expr) shouldBe evaluated(8)
  }

}
