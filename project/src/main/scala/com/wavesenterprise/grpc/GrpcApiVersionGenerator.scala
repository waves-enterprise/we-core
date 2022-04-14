package com.wavesenterprise.grpc

import com.wavesenterprise.wavesenterprise.writeTextFile
import sbt.Keys.{sourceDirectory, sourceGenerators, version}
import sbt._

import java.io.File

object GrpcApiVersionGenerator extends AutoPlugin {
  override lazy val projectSettings = Seq(
    (sourceGenerators in Compile) += codeGenerator
  )

  lazy val codeGenerator = Def.task {
    val path = (sourceDirectory in Compile).value / "protobuf" / "managed"
    generate(path, version.value)
  }

  private def generate(outputDir: File, version: String): Seq[File] = {
    outputDir.mkdirs()

    val apiVersionFile = new File(outputDir, "api_version.proto")
    val text =
      s"""|syntax = "proto3";
          |package wavesenterprise;
          |
          |import "google/protobuf/descriptor.proto";
          |
          |extend google.protobuf.MessageOptions {
          |  string contract_api_version = 51234;
          |}
          |
          |message CurrentContractApiVersion {
          |  option (contract_api_version) = "$version";
          |}""".stripMargin

    writeTextFile(apiVersionFile, text)

    Seq.empty
  }
}
