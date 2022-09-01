package com.wavesenterprise.transaction.generator

import java.io.File
import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.CodeWriter
import com.wavesenterprise.transaction.generator.scala.{AtomicInnerTxAdapterGenerator, TxScalaGenerator}
import com.wavesenterprise.wavesenterprise.writeTextFile
import sbt.Keys.{sourceGenerators, sourceManaged}
import sbt._
import src.main.scala.com.wavesenterprise.transaction.generator.base.Adapter
import src.main.scala.com.wavesenterprise.transaction.generator.scala.TxAdapterGenerator

object TxSchemePlugin extends AutoPlugin {

  override lazy val projectSettings = Seq(
    (Compile / sourceGenerators) += codeGenerator
  )

  lazy val codeGenerator = Def.task {
    val path = (Compile / sourceManaged).value
    generate(path)
  }

  private def generate(outputDir: File): Seq[File] =
    Seq(
      generateAdapter(outputDir, TxAdapterGenerator),
      generateAdapter(outputDir, AtomicInnerTxAdapterGenerator)
    ) ++ generateTransactions(outputDir)

  private def generateTransactions(outputDir: File): Seq[File] = {
    TxScheme.values.map { scheme =>
      val packageDir = new File(outputDir, scheme.packageName.replace('.', '/'))
      packageDir.mkdirs()
      val code = TxScalaGenerator.buildScala(scheme)
      val file = new File(packageDir, s"${scheme.entryName}.scala")
      writeTextFile(file, code)
    }
  }

  private def generateAdapter(outputDir: File, adapter: Adapter): File = {
    val packageDir = new File(outputDir, adapter.packageName.replace('.', '/'))
    packageDir.mkdirs()
    val file = new File(packageDir, s"${adapter.objectName}.scala")
    val code = adapter.buildWriter(TxScheme.values).build()
    writeTextFile(file, code)
    file
  }
}
