package com.wavesenterprise.utils

import cats.kernel.Monoid
import com.wavesenterprise.account.AddressScheme
import com.wavesenterprise.lang.ScriptVersion
import com.wavesenterprise.lang.v1.compiler.CompilerContext
import com.wavesenterprise.lang.v1.evaluator.ctx._
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesenterprise.lang.v1.{CTX, FunctionHeader, ScriptEstimator}
import com.wavesenterprise.transaction.smart.{BlockchainContext, EmptyWavesEnvironment}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import monix.eval.Coeval

import scala.collection.mutable

object SmartContractV1Utils {
  private val lazyAssetContexts: Map[ScriptVersion, Coeval[CTX]] =
    Seq
      .tabulate(2) { v =>
        val version = ScriptVersion.fromInt(v + 1).get
        version -> Coeval.evalOnce(
          Monoid
            .combineAll(Seq(
              PureContext.build(version),
              CryptoContext.build(BlockchainContext.baseGlobal),
              WavesContext
                .build(version, new EmptyWavesEnvironment(AddressScheme.getAddressSchema.chainId), isTokenContext = true)
            )))
      }
      .toMap

  private val lazyContexts: Map[ScriptVersion, Coeval[CTX]] =
    Seq
      .tabulate(2) { v =>
        val version = ScriptVersion.fromInt(v + 1).get
        version -> Coeval.evalOnce(
          Monoid
            .combineAll(Seq(
              PureContext.build(version),
              CryptoContext.build(BlockchainContext.baseGlobal),
              WavesContext.build(version, new EmptyWavesEnvironment(AddressScheme.getAddressSchema.chainId), isTokenContext = false)
            )))
      }
      .toMap

  private val lazyFunctionCosts: Map[ScriptVersion, Coeval[Map[FunctionHeader, Coeval[Long]]]] =
    lazyContexts.mapValues(_.map(ctx => estimate(ctx.evaluationContext)))

  def functionCosts(version: ScriptVersion): Map[FunctionHeader, Coeval[Long]] = lazyFunctionCosts(version)()

  def compilerContext(version: ScriptVersion, isAssetScript: Boolean): CompilerContext =
    if (isAssetScript) lazyAssetContexts(version)().compilerContext
    else lazyContexts(version)().compilerContext

  def varNames(version: ScriptVersion): Set[String] = compilerContext(version, isAssetScript = false).varDefs.keySet

  def dummyEvalContext(version: ScriptVersion): EvaluationContext = lazyContexts(version)().evaluationContext

  def estimate(ctx: EvaluationContext): Map[FunctionHeader, Coeval[Long]] = {
    val costs: mutable.Map[FunctionHeader, Coeval[Long]] = ctx.typeDefs.collect {
      case (typeName, CaseType(_, fields)) => FunctionHeader.User(typeName) -> Coeval.now(fields.size.toLong)
    }(collection.breakOut)

    ctx.functions.values.foreach { func =>
      val cost = func match {
        case f: UserFunction =>
          import f.signature.args
          Coeval.evalOnce(ScriptEstimator(ctx.letDefs.keySet ++ args.map(_._1), costs, f.ev).explicitGet() + args.size * 5)
        case f: NativeFunction => Coeval.now(f.cost)
      }
      costs += func.header -> cost
    }

    costs.toMap
  }

}
