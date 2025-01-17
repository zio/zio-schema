addSbtPlugin("org.scalameta"      % "sbt-scalafmt"                  % "2.5.4")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"                  % "0.14.0")
addSbtPlugin("com.github.cb372"   % "sbt-explicit-dependencies"     % "0.3.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.18.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.13.1")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"                % "1.9.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.5")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                       % "0.4.7")
addSbtPlugin("dev.zio"            % "zio-sbt-website"               % "0.4.0-alpha.30")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"               % "1.1.4")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.9"
