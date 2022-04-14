package com.wavesenterprise.transaction.generator.base

import com.wavesenterprise.transaction.TxScheme

trait ScalaGenerator {

  protected def imports: Set[String] = Set.empty

  protected def buildWriter(scheme: TxScheme): CodeWriter = CodeWriter()

  final def buildScala(scheme: TxScheme): String = buildWriter(scheme).build()
}
