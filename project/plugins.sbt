addSbtPlugin("org.scalameta"      % "sbt-scalafmt"                  % "2.4.5")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"                  % "0.11.1")
addSbtPlugin("com.github.cb372"   % "sbt-explicit-dependencies"     % "0.3.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.16.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.11.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"                % "1.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.5")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                       % "0.4.6")
addSbtPlugin("dev.zio"            % "zio-sbt-website"               % "0.4.0-alpha.22")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.7"
