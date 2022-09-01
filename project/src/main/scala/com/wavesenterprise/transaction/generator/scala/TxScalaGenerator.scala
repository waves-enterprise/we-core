package com.wavesenterprise.transaction.generator.scala

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.{ScalaGenerator, CodeWriter}
import com.wavesenterprise.transaction.generator.parts.{
  CaseClassesCompanionGenerator,
  CaseClassesGenerator,
  SealedTraitCompanionGenerator,
  SealedTraitGenerator
}

object TxScalaGenerator
    extends ScalaGenerator
    with SealedTraitGenerator
    with SealedTraitCompanionGenerator
    with CaseClassesGenerator
    with CaseClassesCompanionGenerator {

  override protected def buildWriter(scheme: TxScheme): CodeWriter = {
    val resultImports = (
      imports ++
        scheme.additionalImports ++
        scheme.fields.flatMap(_.tpe.scalaImports) +
        s"${scheme.protobufPackageName}.{${scheme.entryName} => Pb${scheme.entryName}}" +
        s"${TxScheme.BaseProtoPackage}.{Transaction => PbTransaction}"
    ).diff(scheme.unusedImports).filterNot { imp =>
      // No need to include imports of the current package
      imp.startsWith(scheme.packageName) && !imp.drop(scheme.packageName.length + 1).contains(".")
    }

    CodeWriter()
      .addLines("package " + scheme.packageName)
      .newLine
      .fold(resultImports) {
        case (writer, imp) => writer.addLines("import " + imp)
      }
      .newLine
      .combine(super.buildWriter(scheme))
  }
}
