package com.wavesenterprise.lang.v1.evaluator.ctx

import com.wavesenterprise.lang.v1.compiler.Terms.{CaseObj, EVALUATED}
import com.wavesenterprise.lang.v1.compiler.Types.CASETYPEREF

package object impl {
  def notImplemented(funcName: String, args: List[Any]): Nothing = throw new Exception(
    s"Can't apply (${args.map(_.getClass.getSimpleName).mkString(", ")}) to '$funcName'"
  )

  lazy val UNIT: CASETYPEREF = CASETYPEREF("Unit", List.empty)
  lazy val unit: EVALUATED   = CaseObj(UNIT, Map.empty)
}
