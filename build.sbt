import com.typesafe.sbt.SbtGit.GitKeys.gitUncommittedChanges
import com.typesafe.sbt.git.JGit
import com.wavesenterprise.grpc.GrpcApiVersionGenerator
import com.wavesenterprise.transaction.generator.{TxSchemePlugin, TxSchemeProtoPlugin}
import org.eclipse.jgit.submodule.SubmoduleWalk.IgnoreSubmoduleMode
import sbt.Keys.{credentials, sourceGenerators, _}
import sbt.internal.inc.ReflectUtilities
import sbt.librarymanagement.ivy.IvyDependencyResolution
import sbt.{Compile, Credentials, Def, Path, _}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

import java.io.File

enablePlugins(GitVersioning)
scalafmtOnCompile in ThisBuild := true

Global / cancelable := true
Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  Seq(
    scalaVersion := "2.12.10",
    organization := "com.wavesenterprise",
    organizationName := "wavesenterprise",
    organizationHomepage := Some(url("https://wavesenterprise.com")),
    description := "Library for Waves Enterprise blockchain platform",
    homepage := Some(url("https://github.com/waves-enterprise/we-core")),
    pomIncludeRepository := { _ =>
      false
    },
    publishMavenStyle := true,
    licenses ++= Seq(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")
    ),
    developers ++= List(
      Developer(
        id = "vaan",
        name = "Vadim Anufriev",
        email = "vanufriev@web3tech.ru",
        url = url("https://vaan.io/")
      ),
      Developer(
        id = "squadgazzz",
        name = "Ilia Zhavoronkov",
        email = "izhavoronkov89@list.ru",
        url = url("https://www.linkedin.com/in/ilya-zhavoronkov/")
      ),
      Developer(
        id = "sathembite",
        name = "Anton Mazur",
        email = "sathembite@gmail.com",
        url = url("https://github.com/AntonMazur")
      ),
      Developer(
        id = "kantefier",
        name = "Kirill Nebogin",
        email = "kinebogin@gmail.com",
        url = url("https://github.com/kantefier")
      ),
      Developer(
        id = "gamzaliev",
        name = "Ruslan Gamzaliev",
        email = "gamzaliev.ruslan.94@gmail.com",
        url = url("https://github.com/gamzaliev")
      )
    ),
    crossPaths := false,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Ypartial-unification"
    )
  )
)

fork in run := true

name := "we-core"

/**
  * You have to put your credentials in a local file ~/.sbt/.credentials
  * File structure:
  *
  * realm=Sonatype Nexus Repository Manager
  * host=artifacts.wavesenterprise.com
  * username={YOUR_LDAP_USERNAME}
  * password={YOUR_LDAP_PASSWORD}
  */
credentials += {
  val envUsernameOpt = sys.env.get("nexusUser")
  val envPasswordOpt = sys.env.get("nexusPassword")

  (envUsernameOpt, envPasswordOpt) match {
    case (Some(username), Some(password)) =>
      println("Using credentials from environment for artifacts.wavesenterprise.com")
      Credentials("Sonatype Nexus Repository Manager", "artifacts.wavesenterprise.com", username, password)

    case _ =>
      val localCredentialsFile = Path.userHome / ".sbt" / ".credentials"
      println(s"Going to use ${localCredentialsFile.getAbsolutePath} as credentials for artifacts.wavesenterprise.com")
      Credentials(localCredentialsFile)
  }
}

excludeDependencies ++= Seq(
  // workaround for https://github.com/sbt/sbt/issues/3618
  // include "jakarta.ws.rs" % "jakarta.ws.rs-api" instead
  ExclusionRule("javax.ws.rs", "javax.ws.rs-api")
)

libraryDependencies += "jakarta.ws.rs" % "jakarta.ws.rs-api" % "2.1.5"

val coreVersionSource = Def.task {
  // WARNING!!!
  // Please, update the fallback version every major and minor releases.
  // This version is used then building from sources without Git repository
  // In case of not updating the version cores build from headless sources will fail to connect to newer versions
  val FallbackVersion = (1, 0, 0)

  val coreVersionFile: File = (sourceManaged in Compile).value / "com" / "wavesenterprise" / "CoreVersion.scala"
  val versionExtractor      = """(\d+)\.(\d+)\.(\d+).*""".r
  val (major, minor, patch) = version.value match {
    case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
    case _                            => FallbackVersion
  }

  IO.write(
    coreVersionFile,
    s"""package com.wavesenterprise
       |
       |object CoreVersion {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin
  )

  Seq(coreVersionFile)
}

normalizedName := s"${name.value}"

gitUncommittedChanges in ThisBuild := JGit(baseDirectory.value).porcelain
  .status()
  .setIgnoreSubmodules(IgnoreSubmoduleMode.ALL)
  .call()
  .hasUncommittedChanges

version in ThisBuild := {
  val suffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, Some("DIRTY"))
  val releaseVersion = git.releaseVersion(
    git.gitCurrentTags.value,
    git.gitTagToVersionNumber.value,
    suffix
  )
  lazy val describedExtended = git.gitDescribedVersion.value.map { described =>
    val commitHashLength            = 7
    val tagVersionWithoutCommitHash = described.take(described.length - commitHashLength - 2)
    val tagVersionWithCommitsAhead  = tagVersionWithoutCommitHash.take(tagVersionWithoutCommitHash.lastIndexOf('-'))
    import scala.sys.process._
    val branchName = sys.env.getOrElse("CI_COMMIT_REF_NAME", "git rev-parse --abbrev-ref HEAD".!!.trim)
    s"$tagVersionWithCommitsAhead-$branchName-SNAPSHOT"
  }
  releaseVersion
    .orElse(describedExtended)
    .getOrElse(git.formattedDateVersion.value)
}

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ywarn-unused:-implicits",
  "-Xlint",
  "-Yresolve-term-conflict:object",
  "-Ypartial-unification",
  "-language:postfixOps"
)

scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(true)))

// for sbt plugins sources resolving
updateSbtClassifiers / dependencyResolution := IvyDependencyResolution((updateSbtClassifiers / ivyConfiguration).value)
resolvers ++= Seq(
  Resolver.bintrayRepo("ethereum", "maven"),
  Resolver.bintrayRepo("dnvriend", "maven"),
  Resolver.sbtPluginRepo("releases")
)

javaOptions in run ++= Seq(
  "-XX:+IgnoreUnrecognizedVMOptions"
)

Test / fork := true
Test / javaOptions ++= Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-exports=java.base/jdk.internal.ref=ALL-UNNAMED",
  "-Dnode.waves-crypto=true"
)
Test / parallelExecution := true

inConfig(Compile)(
  Seq(
    sourceGenerators += coreVersionSource
  )
)

inConfig(Test)(
  Seq(
    logBuffered := false,
    parallelExecution := true,
    testListeners := Seq.empty,
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup({ _ =>
      sys.props("sbt-testing") = "true"
    }),
    publishArtifact in packageDoc := false,
    publishArtifact in packageSrc := false,
    publishArtifact in packageBin := false
  ))

// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] =
  ReflectUtilities.allVals[Project](this).values.toList map { p =>
    p: ProjectReference
  }

addCommandAlias(
  "checkPR",
  """;
    |set scalacOptions in ThisBuild ++= Seq("-Xfatal-warnings");
    |Global / checkPRRaw;
    |set scalacOptions in ThisBuild -= "-Xfatal-warnings";
  """.stripMargin
)
lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw in Global := {
  try {
    clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile))).value
  } finally {
    test.all(ScopeFilter(inProjects(langJVM, core), inConfigurations(Test))).value
    (langJS / Compile / fastOptJS).value
  }
}

lazy val publishingRepo: Some[Resolver] =
  Some("releases" at "https://s01.oss.sonatype.org/service/local/staging/deploy/maven2")

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .settings(
      // the following line forces scala version across all dependencies
      scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(true))),
      addCompilerPlugin(Dependencies.kindProjector),
      libraryDependencies ++=
        Dependencies.catsCore ++
          Dependencies.fp ++
          Dependencies.scalacheck ++
          Dependencies.scorex ++
          Dependencies.scalatest ++
          Dependencies.monix.value ++
          Dependencies.scodec.value ++
          Dependencies.fastparse.value,
      resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins"),
      resolvers += Resolver.sbtPluginRepo("releases")
    )
    .settings(
      publishTo := publishingRepo,
      publishConfiguration := publishConfiguration.value.withOverwrite(true),
      publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    )
    .jsSettings(
      scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jvmSettings(
      credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
      name := "RIDE Compiler",
      normalizedName := "lang",
      description := "The RIDE smart contract language compiler",
      licenses := Seq(("MIT", url("https://github.com/wavesplatform/Waves/blob/master/LICENSE"))),
      libraryDependencies ++= Seq(
        "org.scala-js"                      %% "scalajs-stubs" % "1.0.0-RC1" % "provided",
        "com.github.spullara.mustache.java" % "compiler"       % "0.9.5"
      ) ++ Dependencies.logging
        .map(_ % "test") // scrypto logs an error if a signature verification was failed
    )

lazy val langJS = lang.js
lazy val langJVM = lang.jvm
  .dependsOn(crypto)
  .dependsOn(utils)
  .aggregate(crypto, utils)
  .settings(
    moduleName := "we-lang",
  )

lazy val utils = (project in file("utils"))
  .settings(
    moduleName := "we-utils",
    libraryDependencies ++= Seq(
      Dependencies.pureConfig,
      Dependencies.serialization,
      Dependencies.monix.value,
      Dependencies.logging,
      Dependencies.catsCore,
      Dependencies.scorex
    ).flatten,
  )
  .settings(
    publishTo := publishingRepo,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  )

lazy val models = (project in file("models"))
  .enablePlugins(TxSchemePlugin)
  .dependsOn(crypto)
  .dependsOn(langJVM)
  .dependsOn(grpcProtobuf)
  .dependsOn(transactionProtobuf)
  .aggregate(crypto, langJVM, grpcProtobuf, transactionProtobuf)
  .settings(
    moduleName := "we-models",
    Compile / unmanagedSourceDirectories += sourceManaged.value / "main" / "com" / "wavesenterprise" / "models",
    libraryDependencies ++= Seq(
      Dependencies.pureConfig,
      Dependencies.catsCore,
      Dependencies.monix.value,
      Dependencies.protobuf,
      Dependencies.scodec.value,
      Dependencies.serialization,
      Dependencies.commonsNet
    ).flatten
  )
  .settings(
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    publishTo := publishingRepo,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  )

lazy val crypto: Project = project
  .dependsOn(utils)
  .aggregate(utils)
  .settings(
    moduleName := "we-crypto",
    libraryDependencies ++= Seq(
      Dependencies.scorex,
      Dependencies.catsCore,
      Dependencies.logging,
      Dependencies.enumeratum,
      Dependencies.bouncyCastle,
      Dependencies.serialization
    ).flatten,
    publishTo := publishingRepo,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  )

lazy val testCore: Project = (project in file("test-core"))
  .dependsOn(models)
  .aggregate(models)
  .settings(
    moduleName := "we-test-core",
    libraryDependencies ++= Seq(Dependencies.commonsLang, Dependencies.netty).flatten,
    scalacOptions += "-Yresolve-term-conflict:object",
  )
  .settings(
    publishTo := publishingRepo,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  )

val grpcProtobufVersion = "1.5"

lazy val grpcProtobuf = (project in file("grpc-protobuf"))
  .enablePlugins(AkkaGrpcPlugin)
  .enablePlugins(GrpcApiVersionGenerator)
  .dependsOn(transactionProtobuf)
  .aggregate(transactionProtobuf)
  .settings(
    moduleName := "we-grpc-protobuf",
    version := {
      val suffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, Some("DIRTY"))
      grpcProtobufVersion + suffix
    },
    scalacOptions += "-Yresolve-term-conflict:object",
    libraryDependencies ++= Dependencies.protobuf,
  )
  .settings(
    publishTo := publishingRepo,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  )

lazy val transactionProtobuf = (project in file("transaction-protobuf"))
  .enablePlugins(TxSchemeProtoPlugin)
  .enablePlugins(AkkaGrpcPlugin)
  .settings(
    moduleName := "we-transaction-protobuf",
    scalacOptions += "-Yresolve-term-conflict:object",
    libraryDependencies ++= Dependencies.protobuf,
  )
  .settings(
    publishTo := publishingRepo,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  )

lazy val protobufZipTask = taskKey[File]("archive-protobuf")

lazy val protobufZipSetting: Def.Setting[Task[File]] = protobufZipTask := {
  (transactionProtobuf / Compile / compile).value
  (grpcProtobuf / Compile / compile).value

  val txProtoDir   = (sourceDirectory in transactionProtobuf).value / "main" / "protobuf"
  val grpcProtoDir = (sourceDirectory in grpcProtobuf).value / "main" / "protobuf"
  val zipName      = s"we_protobuf_${version.value}.zip"

  IO.delete(new sbt.File("./target/") * "we_protobuf_*.zip" get)

  val filesToZip = for {
    pathFinder    <- Set(txProtoDir, grpcProtoDir)
    directory     <- pathFinder.get()
    fileWithPaths <- Path.selectSubpaths(directory, "*.proto" | "*.md")
  } yield fileWithPaths

  val outputZip = new sbt.File("./target/", zipName)

  IO.zip(filesToZip, outputZip)
  println(s"Protobuf archive has been created: '${outputZip.getAbsolutePath}'")
  outputZip
}

lazy val protobufArchives = (project in file("we-transaction-protobuf"))
  .dependsOn(grpcProtobuf)
  .settings(
    name := "we-transaction-protobuf-archive",
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    publish / skip := true,
    publishArtifact in (Compile, packageSrc) := false,
    publishArtifact in (Compile, packageBin) := false,
    publishArtifact in (Compile, packageDoc) := false,
    protobufZipSetting,
    artifact in (Compile, protobufZipTask) ~= ((art: Artifact) => art.withType("zip").withExtension("zip")),
    addArtifact(artifact in (Compile, protobufZipTask), protobufZipTask)
  )

addCommandAlias(
  "compileAll",
  "; cleanAll; transactionProtobuf/compile; compile; test:compile"
)

lazy val core = project
  .in(file("."))
  .dependsOn(models)
  .dependsOn(testCore % "test->test")
  .aggregate(models)
  .settings(
    moduleName := "we-core",
    addCompilerPlugin(Dependencies.kindProjector),
    libraryDependencies ++= Seq(
      Dependencies.scalatest,
      Dependencies.scalacheck,
      Dependencies.commonsLang,
      Dependencies.docker,
      Dependencies.asyncHttpClient,
      Dependencies.netty
    ).flatten
  )
  .settings(
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    publishTo := publishingRepo,
    publishConfiguration := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  )

lazy val javaHomeProguardOption = Def.task[String] {
  (for {
    versionStr <- sys.props
      .get("java.version")
      .toRight("failed to get system property java.version")
    javaHome <- sys.props
      .get("java.home")
      .toRight("failed to get system property java.home")
    version <- "^(\\d+).*".r
      .findFirstMatchIn(versionStr)
      .map(_.group(1))
      .toRight(s"java.version system property has wrong format: '$versionStr'")
  } yield {
    if (version.toInt > 8)
      s"-libraryjars $javaHome/jmods/java.base.jmod"
    else ""
  }) match {
    case Right(path) => path
    case Left(error) => sys.error(error)
  }
}

lazy val extLibrariesProguardExclusions = Def.task[Seq[String]] {
  libraryDependencies.value
    .map(_.organization.toLowerCase)
    .distinct
    .filterNot(org => org.startsWith("com.wavesenterprise") && org.startsWith("com.wavesplatform"))
    .flatMap { org =>
      Seq(
        s"-keep class $org.** { *; }",
        s"-keep interface $org.** { *; }",
        s"-keep enum $org.** { *; }"
      )
    }
}

/* ********************************************************* */

def printMessage(msg: String): Unit = {
  println(" " + ">" * 3 + " " + msg)
}

def printMessageTask(msg: String) = Def.task {
  printMessage(msg)
}

/* ********************************************************* */

lazy val cleanProtobufManagedDirs = Def.sequential(
  Def.task[Unit] {
    IO.delete((sourceDirectory in transactionProtobuf).value / "main" / "protobuf" / "managed")
  },
  Def.task[Unit] {
    IO.delete((sourceDirectory in grpcProtobuf).value / "main" / "protobuf" / "managed")
  }
)

lazy val cleanAll = taskKey[Unit]("Clean all sub-projects")

cleanAll := Def
  .sequential(
    printMessageTask("Clean grpcProtobuf"),
    clean in grpcProtobuf,
    printMessageTask("Clean transactionProtobuf"),
    clean in transactionProtobuf,
    cleanProtobufManagedDirs,
    printMessageTask("Clean core"),
    clean in core,
    printMessageTask("Clean crypto"),
    clean in crypto,
    printMessageTask("Clean models"),
    clean in models,
    printMessageTask("Clean utils"),
    clean in utils
  )
  .value
