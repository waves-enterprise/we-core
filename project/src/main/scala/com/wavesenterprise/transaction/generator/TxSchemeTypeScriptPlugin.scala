package com.wavesenterprise.transaction.generator

import java.io.File

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.typescript.{TestsTypeScriptGenerator, TxTypeScriptGenerator}
import com.wavesenterprise.wavesenterprise.writeTextFile
import sbt.Keys.{baseDirectory, sourceGenerators}
import sbt._
import src.main.scala.com.wavesenterprise.transaction.generator.typescript.ConstantsTypeScriptGenerator

object TxSchemeTypeScriptPlugin extends AutoPlugin {

  override lazy val projectSettings = Seq(
    (Compile / sourceGenerators) += codeGenerator
  )

  lazy val codeGenerator = Def.task {
    val srcPath  = (Compile / baseDirectory).value / "src"
    val testPath = (Compile / baseDirectory).value / "tests"
    generate(srcPath, testPath)
  }

  private def generate(srcPath: File, testPath: File): Seq[File] = {
    srcPath.mkdirs()
    testPath.mkdirs()

    val txSchemes = TxScheme.values.filter(_.supportedTypeScriptVersions.nonEmpty)
    val txFile    = new File(srcPath, s"${TxTypeScriptGenerator.FILE_NAME}.ts")
    val constFile = new File(srcPath, s"${ConstantsTypeScriptGenerator.FILE_NAME}.ts")
    val testsFile = new File(testPath, s"${TestsTypeScriptGenerator.FILE_NAME}.ts")
    val txsStr    = TxTypeScriptGenerator.buildTypeScript(txSchemes)
    val constsStr = ConstantsTypeScriptGenerator.buildTypeScript(txSchemes)
    val testsStr  = TestsTypeScriptGenerator.buildTypeScript(txSchemes)

    writeTextFile(txFile, txsStr)
    writeTextFile(constFile, constsStr)
    writeTextFile(testsFile, testsStr)

    Seq.empty // The result is typescript files, not scala.
  }
}
