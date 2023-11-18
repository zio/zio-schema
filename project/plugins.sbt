addSbtPlugin("org.scalameta"      % "sbt-scalafmt"                  % "2.5.2")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"                  % "0.11.1")
addSbtPlugin("com.github.cb372"   % "sbt-explicit-dependencies"     % "0.2.10")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.13.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.0")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.11.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"                % "1.5.11")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.1.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.4.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                       % "0.4.3")
addSbtPlugin("dev.zio"            % "zio-sbt-website"               % "0.3.10")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.5"
