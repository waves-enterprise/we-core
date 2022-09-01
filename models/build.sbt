name := "we-models"
Compile / unmanagedSourceDirectories += sourceManaged.value / "main" / "com" / "wavesenterprise" / "models"
libraryDependencies ++= Seq(
  Dependencies.pureConfig,
  Dependencies.catsCore,
  Dependencies.monix.value,
  Dependencies.protobuf,
  Dependencies.scodec.value,
  Dependencies.serialization,
  Dependencies.commonsNet
).flatten
publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
