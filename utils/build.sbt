name := "we-utils"

libraryDependencies ++= Seq(
  Dependencies.pureConfig,
  Dependencies.serialization,
  Dependencies.monix.value,
  Dependencies.logging,
  Dependencies.catsCore,
  Dependencies.scorex
).flatten

publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
