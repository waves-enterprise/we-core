Seq(
  "com.typesafe.sbt"        % "sbt-git"                  % "1.0.0",
  "org.scala-js"            % "sbt-scalajs"              % "0.6.29",
  "org.portable-scala"      % "sbt-crossproject"         % "0.6.1",
  "org.portable-scala"      % "sbt-scalajs-crossproject" % "0.6.1",
  "org.scalameta"           % "sbt-scalafmt"             % "2.4.6",
  "com.github.tkawachi"     % "sbt-repeat"               % "0.1.0",
  "com.codacy"              % "sbt-codacy-coverage"      % "3.0.3",
  "com.lightbend.akka.grpc" % "sbt-akka-grpc"            % "1.1.1",
  "com.github.sbt"          % "sbt-pgp"                  % "2.1.2"
).map(addSbtPlugin)
