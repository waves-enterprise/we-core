import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.toPlatformDepsGroupID
import sbt._

object Dependencies {

  lazy val serialization = Seq(
    "com.google.guava"  % "guava"                     % "28.1-jre",
    "com.typesafe.play" %% "play-json"                % "2.7.4",
    "org.julienrf"      %% "play-json-derived-codecs" % "6.0.0"
  )

  lazy val logging = Seq(
    "org.slf4j" % "slf4j-api" % "1.7.26"
  )

  lazy val meta  = Seq("com.chuusai" %% "shapeless" % "2.3.3")
  lazy val monix = Def.setting(Seq("io.monix" %%% "monix" % "3.1.0"))

  lazy val scodec    = Def.setting(Seq("org.scodec" %%% "scodec-core" % "1.10.3"))
  lazy val fastparse = Def.setting(Seq("com.lihaoyi" %%% "fastparse" % "2.2.4"))
  lazy val ficus     = Seq("com.iheart" %% "ficus" % "1.4.7")
  lazy val scorex = Seq(
    ("org.scorexfoundation" %% "scrypto" % "2.1.6")
      .exclude("ch.qos.logback", "logback-classic")
      .exclude("com.typesafe.scala-logging", "scala-logging_2.12")
      .exclude("com.google.guava", "guava")
      .exclude("org.bouncycastle", "*"),
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
  )

  lazy val bouncyCastle = Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.60",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.60"
  )

  lazy val commonsNet = Seq("commons-net" % "commons-net" % "3.6")
  lazy val commonsLang = Seq(
    "org.apache.commons" % "commons-lang3" % "3.8",
    "commons-codec"      % "commons-codec" % "1.11"
  )

  lazy val scalatest             = Seq("org.scalatest"          %% "scalatest"               % "3.0.8")
  lazy val scalacheck            = Seq("org.scalacheck"         %% "scalacheck"              % "1.14.1")
  lazy val scalaCollectionCompat = Seq("org.scala-lang.modules" %% "scala-collection-compat" % "2.6.0")

  lazy val asyncHttpClient = Seq("org.asynchttpclient" % "async-http-client" % "2.10.5")

  lazy val docker = Seq(
    "com.github.docker-java" % "docker-java-core" % "3.3.0",
    "com.github.docker-java" % "docker-java-transport-httpclient5" % "3.3.0"
  )

  lazy val netty = Seq("io.netty" % s"netty-common" % "4.1.45.Final")

  lazy val catsCore   = Seq("org.typelevel" %% "cats-core" % "2.0.0")
  lazy val catsEffect = Seq("org.typelevel" %% "cats-effect" % "2.0.0")
  lazy val catsMtl    = Seq("org.typelevel" %% "cats-mtl-core" % "0.7.0")
  lazy val fp         = catsCore ++ catsEffect ++ catsMtl

  lazy val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.10"
  lazy val enumeratum    = Seq("com.beachape" %% "enumeratum-play-json" % "1.5.16")

  lazy val pureConfig = Seq(
    "com.github.pureconfig" %% "pureconfig"            % "0.12.2",
    "com.github.pureconfig" %% "pureconfig-enumeratum" % "0.12.2"
  )

  lazy val protobuf = Seq("com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf")

  lazy val reflections= Seq("org.reflections" % "reflections" % "0.10.2")
}
