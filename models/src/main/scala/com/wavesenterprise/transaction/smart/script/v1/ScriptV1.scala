package com.wavesenterprise.transaction.smart.script.v1

import com.wavesenterprise.crypto
import com.wavesenterprise.lang.ScriptVersion
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.v1.compiler.Terms._
import com.wavesenterprise.lang.v1.evaluator.FunctionIds._
import com.wavesenterprise.lang.v1.{FunctionHeader, ScriptEstimator, Serde}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.smart.script.Script
import com.wavesenterprise.utils.SmartContractV1Utils._
import monix.eval.Coeval

object ScriptV1 {
  private val checksumLength = 4
  private val maxComplexity  = 20 * functionCosts(V1)(FunctionHeader.Native(SIGVERIFY))()
  private val maxSizeInBytes = 8 * 1024

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= maxSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $maxSizeInBytes bytes")

  def apply(x: EXPR): Either[String, Script] = apply(V1, x)

  def apply(version: ScriptVersion, x: EXPR, checkSize: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(varNames(version), functionCosts(version), x)
      _                <- Either.cond(scriptComplexity <= maxComplexity, (), s"Script is too complex: $scriptComplexity > $maxComplexity")
      s = new ScriptV1(version, x)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  private class ScriptV1[V <: ScriptVersion](override val version: V, override val expr: EXPR) extends Script {
    override type Ver = V
    override val text: String = expr.toString
    override val bytes: Coeval[ByteStr] =
      Coeval.evalOnce {
        val s = Array(version.value.toByte) ++ Serde.serialize(expr)
        ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
      }
  }
}
