package com.wavesenterprise.transaction.generator.parts

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.CodeWriter
import com.wavesenterprise.transaction.generator.base.ScalaGenerator

trait SealedTraitCompanionGenerator extends ScalaGenerator {

  override def imports: Set[String] = super.imports ++ Set(
    "com.wavesenterprise.transaction.ValidationError"
  )

  override protected def buildWriter(scheme: TxScheme): CodeWriter = {
    super
      .buildWriter(scheme)
      .addLines(s"object ${scheme.entryName} {")
      .indent
      .addLines(s"val typeId: Byte = ${scheme.typeId}")
      .newLine
      .call(buildFromProto(scheme))
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildFromProto(scheme: TxScheme): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"def fromProto(version: Int, tx: Pb${scheme.entryName}): Either[ValidationError, ${scheme.entryName}] = {")
      .indent
      .addLines("version match {")
      .indent
      .fold(scheme.supportedVersions) {
        case (writer, version) =>
          writer.addLines(s"case $version => ${scheme.entryName}V$version.fromProto(tx)")
      }
      .addLines("case _ => Left(ValidationError.UnsupportedVersion(version))")
      .outdent
      .addLines("}")
      .outdent
      .addLines("}")
      .newLine
  }
}
