package com.wavesenterprise.transaction.generator.proto

import com.google.common.base.CaseFormat
import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.CodeWriter

object AggregatedTxsProtoGenerator {
  private val additionalTxsWithImports = List(
    "GenesisTransaction"             -> "genesis/genesis_transaction.proto",
    "GenesisPermitTransaction"       -> "genesis/genesis_permit_transaction.proto",
    "GenesisRegisterNodeTransaction" -> "genesis/genesis_register_node_transaction.proto"
  )

  def buildWriter(schemes: Seq[TxScheme], name: String = "Transaction", skipContainers: Boolean = false): CodeWriter = {
    CodeWriter()
      .addLines("""syntax = "proto3";""")
      .addLines("package wavesenterprise;")
      .newLine
      .addLines(s"""option java_package = "com.wavesenterprise.transaction.protobuf";""")
      .addLines(s"""option csharp_namespace = "WavesEnterprise";""")
      .addLines(s"""option go_package = "wavesenterprise.com/weproto";""")
      .newLine
      .fold(additionalTxsWithImports) {
        case (writer, (_, importPath)) => writer.addLines(s"""import "$importPath";""")
      }
      .fold(schemes) {
        case (writer, scheme) =>
          writer.applyIf(!scheme.isContainer || !skipContainers) {
            _.addLines(s"""import "managed/${scheme.snakeCaseEntryName}.proto";""")
          }
      }
      .newLine
      .addLines(s"message $name {")
      .indent
      .addLines("int32 version = 1;")
      .addLines("oneof transaction {")
      .foldWithAccumulatorIndented(additionalTxsWithImports, 1000) {
        case ((writer, i), (txName, _)) =>
          writer.addLines(s"$txName ${CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, txName)} = $i;") -> (i + 1)
      }
      .foldWithAccumulatorIndented(schemes, 1000 + additionalTxsWithImports.size) {
        case ((writer, i), scheme) =>
          writer.applyIf(!scheme.isContainer || !skipContainers) {
            _.addLines(s"${scheme.entryName} ${scheme.snakeCaseEntryName} = $i;")
          } -> (i + 1)
      }
      .addLines("}")
      .outdent
      .addLines("}")
      .newLine
  }
}
