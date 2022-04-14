package com.wavesenterprise.lang

import cats.kernel.Monoid
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.v1.compiler.CompilerV1
import com.wavesenterprise.lang.v1.compiler.Terms.EXPR
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.waves.WavesContext

object JavaAdapter {
  private val version = V1

  lazy val compiler =
    new CompilerV1(
      Monoid.combineAll(Seq(
        CryptoContext.compilerContext(com.wavesenterprise.lang.Global),
        WavesContext.build(version, null, false).compilerContext,
        PureContext.build(version).compilerContext
      )))

  def compile(input: String): EXPR = {
    compiler
      .compile(input, List())
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
