name := "we-crypto"
scalacOptions += "-Yresolve-term-conflict:object"
libraryDependencies ++= Seq(
  Dependencies.scorex,
  Dependencies.catsCore,
  Dependencies.logging,
  Dependencies.enumeratum,
  Dependencies.bouncyCastle,
  Dependencies.serialization,
  Dependencies.scalaCollectionCompat,
  Dependencies.reflections,
  Dependencies.commonsLang
).flatten
publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
