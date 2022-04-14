package com.wavesenterprise.transaction.generator.parts

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.CodeWriter
import com.wavesenterprise.transaction.generator.base.ScalaGenerator

trait SealedTraitGenerator extends ScalaGenerator {

  override protected def imports: Set[String] = super.imports ++ Set(
    "com.wavesenterprise.transaction.ProvenTransaction",
    "com.wavesenterprise.transaction.FastHashId",
    "com.wavesenterprise.transaction.VersionedTransaction",
    "com.wavesenterprise.transaction.ProtoSerializableTransaction"
  )

  override protected def buildWriter(scheme: TxScheme): CodeWriter = {
    val commonFields = scheme.fields.filter { field =>
      !field.excludeFormSealedTrait &&
      (
        field.isEssential ||
        field.inConstructorVersions.forall { versions =>
          scheme.supportedVersions.forall(v => versions.contains(v) || field.versionToBodyValue.isDefinedAt(v))
        }
      )
    }

    val extensions = Seq(
      "ProvenTransaction",
      "FastHashId",
      "VersionedTransaction",
      "ProtoSerializableTransaction"
    ) ++ scheme.sealedTraitExtensions

    super
      .buildWriter(scheme)
      .addLines(s"sealed trait ${scheme.entryName} extends ${extensions.mkString(" with ")} {")
      .indent
      .fold(commonFields) {
        case (writer, field) =>
          writer.addLines(s"def ${field.name}: ${field.tpe.scalaType}")
      }
      .addLines(s"def toInnerProto: Pb${scheme.entryName}")
      .outdent
      .addLines("}")
      .newLine
  }
}
