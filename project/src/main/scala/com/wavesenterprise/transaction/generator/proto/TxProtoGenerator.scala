package com.wavesenterprise.transaction.generator.proto

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.{CodeWriter, EssentialFields, ProtoGenerator}

object TxProtoGenerator extends ProtoGenerator {

  override protected def buildWriter(scheme: TxScheme): CodeWriter = {
    val dataFields = EssentialFields.id +: scheme.fields.filter { field =>
      field.inConstructorVersions.forall(_.nonEmpty) && // Include field if it belongs to at least one version
      !scheme.supportedVersions.forall(field.versionToBodyValue.isDefinedAt) // Exclude hardcoded fields
    }

    val dataFieldsImports = dataFields.flatMap(_.tpe.protoImports).toSet

    CodeWriter()
      .addLines("""syntax = "proto3";""")
      .addLines("package wavesenterprise;")
      .newLine
      .addLines("option java_multiple_files = true;")
      .addLines(s"""option java_package = "${scheme.protobufPackageName}";""")
      .addLines(s"""option csharp_namespace = "WavesEnterprise";""")
      .addLines(s"""option go_package = "wavesenterprise.com/weproto";""")
      .applyIf(dataFieldsImports.nonEmpty) { writer =>
        writer.newLine
          .fold(dataFieldsImports) {
            case (writer, imp) =>
              writer.addLines(s"""import "$imp";""")
          }
      }
      .newLine
      .addLines(s"message ${scheme.entryName} {")
      .foldWithAccumulatorIndented(dataFields, 1) {
        case ((writer, i), field) =>
          writer.addLines(s"${field.tpe.protoType} ${field.protoSnakeCaseName} = $i;") -> (i + 1)
      }
      .addLines("}")
      .combine(super.buildWriter(scheme))
  }
}
