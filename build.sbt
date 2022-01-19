import sbtcrossproject.CrossPlugin.autoImport.crossProject
import BuildHelper.{ crossProjectSettings, _ }
import explicitdeps.ExplicitDepsPlugin.autoImport.{ moduleFilterRemoveValue, unusedCompileDependenciesFilter }
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSUseMainModuleInitializer
import sbt.moduleFilter

inThisBuild(
  List(
    name := "zio-schema",
    organization := "dev.zio",
    homepage := Some(url("https://github.com/zio/zio-schema")),
    developers := List(
      Developer(
        "ioleo",
        "Piotr Gołębiewski",
        "ioleo+zio@protonmail.com",
        url("https://github.com/ioleo")
      ),
      Developer(
        "jczuchnowski",
        "Jakub Czuchnowski",
        "jakub.czuchnowski@gmail.com",
        url("https://github.com/jczuchnowski")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        homepage.value.get,
        "scm:git:git@github.com:zio/zio-schema.git"
      )
    ),
    licenses := Seq("Apache-2.0" -> url(s"${scmInfo.value.map(_.browseUrl).get}/blob/v${version.value}/LICENSE")),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc")
  )
)

ThisBuild / publishTo := sonatypePublishToBundle.value

addCommandAlias("prepare", "fix; fmt")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("fix", "scalafixAll")
addCommandAlias("fixCheck", "scalafixAll --check")

lazy val root = project
  .in(file("."))
  .settings(
    name := "zio-schema",
    publish / skip := true
//    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    zioSchemaJVM,
    zioSchemaJS,
    zioSchemaDerivationJVM,
    zioSchemaDerivationJS,
    zioSchemaJsonJVM,
    zioSchemaJsonJS,
    zioSchemaOpticsJS,
    zioSchemaOpticsJVM,
    zioSchemaProtobufJS,
    zioSchemaProtobufJVM,
    zioSchemaExamplesJS,
    zioSchemaExamplesJVM,
    testsJVM,
    testsJS,
    zioSchemaZioTestJVM,
    zioSchemaZioTestJS
  )

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .in(file("tests"))
  .dependsOn(zioSchemaDerivation % "compile->test", zioSchema % "test->test", zioSchemaZioTest % "compile->test")
  .settings(stdSettings("zio-schema-tests"))
  .settings(publish / skip := true)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))

lazy val testsJS = tests.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val testsJVM = tests.jvm
  .settings(Test / fork := true)

lazy val zioSchema = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema"))
  .settings(stdSettings("zio-schema"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"         % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-prelude" % zioPreludeVersion
    )
  )

lazy val zioSchemaJS = zioSchema.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaJVM = zioSchema.jvm
  .settings(Test / fork := true)

lazy val zioSchemaDerivation = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-derivation"))
  .dependsOn(zioSchema, zioSchema, zioSchema % "test->test")
  .settings(stdSettings("zio-schema-derivation"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"         % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-prelude" % zioPreludeVersion
    )
  )
  .settings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) =>
          Seq(
            "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
          )
        case _ => Seq()
      }
    }
  )

lazy val zioSchemaDerivationJS = zioSchemaDerivation.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaDerivationJVM = zioSchemaDerivation.jvm
  .settings(Test / fork := true)

lazy val zioSchemaJson = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-json"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-json"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.json"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json" % zioJsonVersion
    )
  )

lazy val zioSchemaJsonJS = zioSchemaJson.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaJsonJVM = zioSchemaJson.jvm
  .settings(Test / fork := true)

lazy val zioSchemaProtobuf = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-protobuf"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-protobuf"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.protobuf"))

lazy val zioSchemaProtobufJS = zioSchemaProtobuf.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaProtobufJVM = zioSchemaProtobuf.jvm
  .settings(Test / fork := true)

lazy val zioSchemaOptics = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-optics"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-optics"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.optics"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-optics" % zioOpticsVersion
    )
  )

lazy val zioSchemaOpticsJS = zioSchemaOptics.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaOpticsJVM = zioSchemaOptics.jvm
  .settings(Test / fork := true)

lazy val zioSchemaExamples = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-examples"))
  .settings(stdSettings("zio-schema-examples"))
  .settings(crossScalaVersions -= Scala212)
  .dependsOn(zioSchema, zioSchemaJson, zioSchemaProtobuf, zioSchemaOptics)
  .settings(
    publish / skip := true,
    moduleName := "zio-schema-example",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings"
  )

lazy val zioSchemaExamplesJS = zioSchemaExamples.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaExamplesJVM = zioSchemaExamples.jvm

lazy val zioSchemaZioTest = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-zio-test"))
  .dependsOn(zioSchema, zioSchemaDerivation)
  .settings(stdSettings("zio-schema-zio-test"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.test"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test" % zioVersion
    )
  )

lazy val zioSchemaZioTestJS = zioSchemaZioTest.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaZioTestJVM = zioSchemaZioTest.jvm
  .settings(Test / fork := true)

lazy val docs = project
  .in(file("zio-schema-docs"))
  .settings(
    publish / skip := true,
    mdocVariables := Map(
      "SNAPSHOT_VERSION" -> version.value,
      "RELEASE_VERSION"  -> previousStableVersion.value.getOrElse("can't find release"),
      "ORG"              -> organization.value,
      "NAME"             -> (zioSchemaJVM / name).value,
      "CROSS_VERSIONS"   -> (zioSchemaJVM / crossScalaVersions).value.mkString(", ")
    ),
    moduleName := "zio-schema-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    ),
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(root),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(zioSchemaJVM)
  .enablePlugins(JmhPlugin)
