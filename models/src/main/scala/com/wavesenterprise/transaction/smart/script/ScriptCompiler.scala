package com.wavesenterprise.transaction.smart.script

import cats.implicits._
import com.wavesenterprise.lang.ScriptVersion
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.directives.{Directive, DirectiveKey, DirectiveParser}
import com.wavesenterprise.lang.v1.ScriptEstimator
import com.wavesenterprise.lang.v1.compiler.CompilerV1
import com.wavesenterprise.lang.v1.compiler.Terms.EXPR
import com.wavesenterprise.transaction.smart.script.v1.ScriptV1
import com.wavesenterprise.utils.SmartContractV1Utils._
import com.wavesenterprise.utils.ScorexLogging

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object ScriptCompiler extends ScorexLogging {

  def apply(scriptText: String, isAssetScript: Boolean): Either[String, (Script, Long)] = {
    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.linesIterator
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      ver        <- extractVersion(directives)
      expr       <- tryCompile(scriptWithoutDirectives, ver, isAssetScript, directives)
      script     <- ScriptV1(ver, expr)
      complexity <- ScriptEstimator(varNames(ver), functionCosts(ver), expr)
    } yield (script, complexity)
  }

  def tryCompile(src: String, version: ScriptVersion, isAssetScript: Boolean, directives: List[Directive]): Either[String, EXPR] = {
    val compiler = new CompilerV1(compilerContext(version, isAssetScript))
    try {
      compiler.compile(src, directives)
    } catch {
      case NonFatal(ex) =>
        log.error("Error compiling script", ex)
        log.error(src)
        val msg = Option(ex.getMessage).getOrElse("Parsing failed: Unknown error")
        Left(msg)
    }
  }

  def estimate(script: Script, version: ScriptVersion): Either[String, Long] = script match {
    case Script.Expr(expr) => ScriptEstimator(varNames(version), functionCosts(version), expr)
  }

  private def extractVersion(directives: List[Directive]): Either[String, ScriptVersion] = {
    directives
      .find(_.key == DirectiveKey.LANGUAGE_VERSION)
      .map(d =>
        Try(d.value.toInt) match {
          case Success(v) =>
            ScriptVersion
              .fromInt(v)
              .fold[Either[String, ScriptVersion]](Left("Unsupported language version"))(_.asRight)
          case Failure(_) =>
            Left("Can't parse language version")
        })
      .getOrElse(V1.asRight)
  }

}
