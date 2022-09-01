package com.wavesenterprise.transaction.generator

import java.io.File

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.proto.{AggregatedTxsProtoGenerator, TxProtoGenerator}
import com.wavesenterprise.wavesenterprise.writeTextFile
import sbt.Keys.{sourceDirectory, sourceGenerators}
import sbt._

object TxSchemeProtoPlugin extends AutoPlugin {

  override lazy val projectSettings = Seq(
    (Compile / sourceGenerators) += codeGenerator
  )

  lazy val codeGenerator = Def.task {
    val path = (Compile / sourceDirectory).value / "protobuf" / "managed"
    generate(path)
  }

  private def generate(outputDir: File): Seq[File] = {
    outputDir.mkdirs()

    val aggregatedTxsFile = new File(outputDir, s"transaction.proto")
    val aggregatedTxsCode = AggregatedTxsProtoGenerator.buildWriter(TxScheme.values).build()
    writeTextFile(aggregatedTxsFile, aggregatedTxsCode)

    val aggregatedInnerTxFile = new File(outputDir, "atomic_inner_transaction.proto")
    val aggregatedInnerTxCode = AggregatedTxsProtoGenerator.buildWriter(TxScheme.values, "AtomicInnerTransaction", skipContainers = true).build()
    writeTextFile(aggregatedInnerTxFile, aggregatedInnerTxCode)

    TxScheme.values.map { scheme =>
      val txFile = new File(outputDir, s"${scheme.snakeCaseEntryName}.proto")
      val txCode = TxProtoGenerator.buildProto(scheme)
      writeTextFile(txFile, txCode)
    }

    Seq.empty // The result is proto files, not scala.
  }
}
