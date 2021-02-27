import sbt._
import Keys._

import explicitdeps.ExplicitDepsPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport.CrossType
import dotty.tools.sbtplugin.DottyPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbtbuildinfo._
import BuildInfoKeys._
import scalafix.sbt.ScalafixPlugin.autoImport._

object BuildHelper {

  // Keep this consistent with the version in .circleci/config.yml
  val Scala211   = "2.11.12"
  val Scala212   = "2.12.13"
  val Scala213   = "2.13.4"
  val ScalaDotty = "3.0.0-M3"

  val zioVersion      = "1.0.4-2"
  val zioNioVersion   = "1.0.0-RC9"
  val silencerVersion = "1.7.2"
  val magnoliaVersion = "0.16.0"

  private val testDeps = Seq(
    "dev.zio" %% "zio-test"     % zioVersion % "test",
    "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
  )

  private def compileOnlyDeps(scalaVersion: String) = {
    val stdCompileOnlyDeps = Seq(
      )
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, x)) if x <= 12 =>
        stdCompileOnlyDeps ++ Seq(
          compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
        )
      case Some((2, _)) =>
        stdCompileOnlyDeps ++ Seq(
          ("com.github.ghik" % "silencer-lib" % silencerVersion % Provided).cross(CrossVersion.full),
          compilerPlugin(("com.github.ghik" % "silencer-plugin" % silencerVersion).cross(CrossVersion.full)),
          compilerPlugin(("org.typelevel"   %% "kind-projector" % "0.11.1").cross(CrossVersion.full))
        )
      case _ => stdCompileOnlyDeps
    }
  }

  val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked"
  ) ++ {
    if (sys.env.contains("CI")) {
      Seq("-Xfatal-warnings")
    } else {
      Nil // to enable Scalafix locally
    }
  }

  val std2xOptions = Seq(
    "-Xfatal-warnings",
    "-language:higherKinds",
    "-language:existentials",
    "-explaintypes",
    "-Yrangepos",
    "-Xsource:2.13",
    "-Xlint:_,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )

  val dottySettings = Seq(
    crossScalaVersions += ScalaDotty,
    scalacOptions ++= {
      if (isDotty.value)
        Seq("-noindent")
      else
        Seq()
    },
    scalacOptions --= {
      if (isDotty.value)
        Seq("-Xfatal-warnings")
      else
        Seq()
    },
    sources in (Compile, doc) := {
      val old = (Compile / doc / sources).value
      if (isDotty.value) {
        Nil
      } else {
        old
      }
    },
    parallelExecution in Test := {
      val old = (Test / parallelExecution).value
      if (isDotty.value) {
        false
      } else {
        old
      }
    }
  )

  private def optimizerOptions(optimize: Boolean) =
    if (optimize)
      Seq(
        "-opt:l:inline",
        "-opt-inline-from:zio.internal.**"
      )
    else Nil

  def extraOptions(scalaVersion: String, isDotty: Boolean, optimize: Boolean) =
    CrossVersion.partialVersion(scalaVersion) match {
      case _ if isDotty =>
        Seq(
          "-language:implicitConversions",
          "-Xignore-scala2-macros"
        )
      case Some((2, 13)) =>
        Seq(
          "-Ywarn-unused:params,-implicits"
        ) ++ std2xOptions ++ optimizerOptions(optimize)
      case Some((2, 12)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:_,imports",
          "-Ywarn-unused:imports",
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Ywarn-unused:params,-implicits",
          "-Xfuture",
          "-Xsource:2.13",
          "-Xmax-classfile-name",
          "242"
        ) ++ std2xOptions ++ optimizerOptions(optimize)
      case Some((2, 11)) =>
        Seq(
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Xexperimental",
          "-Ywarn-unused-import",
          "-Xfuture",
          "-Xsource:2.13",
          "-Xmax-classfile-name",
          "242"
        ) ++ std2xOptions

      case _ => Seq.empty
    }

  def macroExpansionSettings = Seq(
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-Ymacro-annotations")
        case _             => Seq.empty
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, x)) if x <= 12 =>
          Seq(compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)))
        case _ => Seq.empty
      }
    }
  )

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= {
      if (isDotty.value) Seq()
      else
        Seq(
          "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided",
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
        )
    }
  )

  val buildInfoSettings = Seq(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, isSnapshot),
    buildInfoPackage := "zio",
    buildInfoObject := "BuildInfoZioMacros"
  )

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*) =
    for {
      platform <- List("shared", platform)
      version  <- "scala" :: versions.toList.map("scala-" + _)
      result   = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
      if result.exists
    } yield result

  def crossPlatformSources(scalaVer: String, platform: String, conf: String, baseDir: File, isDotty: Boolean) = {
    val versions = CrossVersion.partialVersion(scalaVer) match {
      case Some((2, 11)) =>
        List("2.11", "2.11+", "2.11-2.12", "2.x")
      case Some((2, 12)) =>
        List("2.12", "2.11+", "2.12+", "2.11-2.12", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.11+", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case _ if isDotty =>
        List("dotty", "2.11+", "2.12+", "2.13+", "3.x")
      case _ =>
        List()
    }
    platformSpecificSources(platform, conf, baseDir)(versions: _*)
  }

  def stdSettings(prjName: String) =
    Seq(
      name := s"$prjName",
      crossScalaVersions := Seq("2.13.4", "2.12.13", "2.11.12", "3.0.0-M3"),
      // scalaVersion in ThisBuild := crossScalaVersions.value.last,
      scalaVersion in ThisBuild := crossScalaVersions.value.head,
      //scalacOptions := compilerOptions(scalaVersion.value, optimize = !isSnapshot.value),
      scalacOptions := stdOptions ++ extraOptions(scalaVersion.value, isDotty.value, optimize = !isSnapshot.value),
      libraryDependencies ++= compileOnlyDeps(scalaVersion.value) ++ testDeps,
      ThisBuild / semanticdbEnabled := !isDotty.value,
      ThisBuild / semanticdbOptions += "-P:semanticdb:synthetics:on",
      ThisBuild / semanticdbVersion := scalafixSemanticdb.revision,
      ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value),
      ThisBuild / scalafixDependencies ++= List(
        "com.github.liancheng" %% "organize-imports" % "0.4.4",
        "com.github.vovapolu"  %% "scaluzzi"         % "0.1.17"
      ),
      parallelExecution in Test := true,
      incOptions ~= (_.withLogRecompileOnMacro(true)),
      autoAPIMappings := true,
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
    )
}
