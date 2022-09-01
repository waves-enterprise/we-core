name := "we-grpc-protobuf"
libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.4.3" cross CrossVersion.full),
  "com.github.ghik" % "silencer-lib" % "1.4.3" % Provided cross CrossVersion.full
) ++ Dependencies.protobuf

scalacOptions += "-P:silencer:globalFilters=Marked as deprecated in proto file"

akkaGrpcGeneratedSources := Seq(AkkaGrpc.Server, AkkaGrpc.Client)
akkaGrpcCodeGeneratorSettings += "server_power_apis"

scalacOptions += "-Yresolve-term-conflict:object"
publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
