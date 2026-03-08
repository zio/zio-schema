// build.sbt

import sbt.Keys._
import sbt._

lazy val zioSchemaJVM = project
  .in(file("."))
  .settings(
    scalaVersion := "3.3.0",  // Ensure you are using Scala 3
    Compile / doc / scalacOptions := 
      // Only include these options if not running on Windows
      (if (sys.props("os.name").toLowerCase.contains("win")) Nil else Seq("-doc-title", "Zio Schema Docs")),
    // Disable doc generation when running publishLocal on Windows
    publishLocal := {
      if (sys.props("os.name").toLowerCase.contains("win")) {
        // Disable doc generation
        (Compile / doc).value
      } else {
        (publishLocal).value
      }
    }
  )