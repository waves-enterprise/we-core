libraryDependencies ++= Seq(
  "com.beachape"     %% "enumeratum" % "1.5.15",
  "com.google.guava" % "guava"       % "28.1-jre"
)

Compile / unmanagedJars ++= (baseDirectory.value / ".." / "crypto" / "lib" ** "*.jar").classpath