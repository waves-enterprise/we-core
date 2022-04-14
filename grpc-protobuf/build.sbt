libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.4.3" cross CrossVersion.full),
  "com.github.ghik" % "silencer-lib" % "1.4.3" % Provided cross CrossVersion.full
)

scalacOptions += "-P:silencer:globalFilters=Marked as deprecated in proto file"

akkaGrpcGeneratedSources := Seq(AkkaGrpc.Server, AkkaGrpc.Client)
akkaGrpcCodeGeneratorSettings += "server_power_apis"
