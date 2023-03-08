import sbtcrossproject.CrossPlugin.autoImport.crossProject
import BuildHelper.{ crossProjectSettings, _ }
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSUseMainModuleInitializer

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.dev/zio-schema")),
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
    licenses := Seq("Apache-2.0" -> url(s"${scmInfo.value.map(_.browseUrl).get}/blob/v${version.value}/LICENSE"))
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
    zioSchemaMacros.js,
    zioSchemaMacros.jvm,
    zioSchemaMacros.native,
    zioSchema.jvm,
    zioSchema.js,
    zioSchema.native,
    zioSchemaDerivation.jvm,
    zioSchemaDerivation.js,
    zioSchemaDerivation.native,
    zioSchemaJsonJVM,
    zioSchemaJsonJS,
    zioSchemaOptics.js,
    zioSchemaOptics.jvm,
    zioSchemaOptics.native,
    zioSchemaProtobufJS,
    zioSchemaProtobufJVM,
    zioSchemaExamples.js,
    zioSchemaExamples.jvm,
    tests.js,
    tests.jvm,
    tests.native,
    zioSchemaZioTest.js,
    zioSchemaZioTest.jvm,
    zioSchemaZioTest.native,
    zioSchemaThriftJS,
    zioSchemaThriftJVM,
    zioSchemaAvroJS,
    zioSchemaAvroJVM,
    zioSchemaMsgPackJS,
    zioSchemaMsgPackJVM,
    docs
  )

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("tests"))
  .dependsOn(zioSchemaDerivation % "compile->test", zioSchema % "test->test", zioSchemaZioTest % "compile->test")
  .settings(stdSettings("zio-schema-tests"))
  .settings(publish / skip := true)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaMacros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-macros"))
  .settings(stdSettings("zio-schema-macros"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(macroDefinitionSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchema = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema"))
  .settings(stdSettings("zio-schema"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"                % zioVersion,
      "dev.zio" %%% "zio-streams"        % zioVersion,
      "dev.zio" %%% "zio-prelude"        % zioPreludeVersion,
      "dev.zio" %%% "zio-constraintless" % zioConstraintlessVersion
    )
  )
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .nativeSettings(nativeSettings)
  .dependsOn(zioSchemaMacros)
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaDerivation = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-derivation"))
  .dependsOn(zioSchema, zioSchema % "test->test")
  .settings(stdSettings("zio-schema-derivation"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"         % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion,
      "dev.zio" %%% "zio-prelude" % zioPreludeVersion
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
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaJson = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-json"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-json"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.json"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % zioJsonVersion
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaJsonJS = zioSchemaJson.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaJsonJVM = zioSchemaJson.jvm

lazy val zioSchemaProtobuf = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-protobuf"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-protobuf"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.protobuf"))
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaProtobufJS = zioSchemaProtobuf.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaProtobufJVM = zioSchemaProtobuf.jvm

lazy val zioSchemaThrift = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-thrift"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-thrift"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.thrift"))
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.thrift" % "libthrift" % "0.16.0"
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaThriftJS = zioSchemaThrift.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaThriftJVM = zioSchemaThrift.jvm

lazy val zioSchemaMsgPack = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-msg-pack"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-msg-pack"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.msgpack"))
  .settings(
    libraryDependencies ++= Seq(
      "org.msgpack"                  % "msgpack-core"               % "0.9.3",
      "org.msgpack"                  % "jackson-dataformat-msgpack" % "0.9.3" % Test,
      "com.fasterxml.jackson.module" %% "jackson-module-scala"      % "2.13.2" % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaMsgPackJS = zioSchemaMsgPack.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaMsgPackJVM = zioSchemaMsgPack.jvm

lazy val zioSchemaAvro = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-avro"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-avro"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.avro"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"         %%% "zio-json" % zioJsonVersion,
      "org.apache.avro" % "avro"       % avroVersion
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioSchemaAvroJS = zioSchemaAvro.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaAvroJVM = zioSchemaAvro.jvm

lazy val zioSchemaOptics = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-optics"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-optics"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.optics"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-optics" % zioOpticsVersion
    )
  )
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

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
  .jsSettings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaZioTest = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-zio-test"))
  .dependsOn(zioSchema, zioSchemaDerivation)
  .settings(stdSettings("zio-schema-zio-test"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.test"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-test" % zioVersion
    )
  )
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(zioSchema.jvm, zioSchemaDerivation.jvm, zioSchemaProtobufJVM)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("zio-schema-docs"))
  .settings(stdSettings("zio-schema-docs"))
  .settings(
    moduleName := "zio-schema-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    ),
    projectName := "ZIO Schema",
    mainModuleName := (zioSchema.jvm / moduleName).value,
    projectStage := ProjectStage.Development,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(),
    docsPublishBranch := "main",
    readmeContribution +=
      """|
         |#### TL;DR
         |
         |Before you submit a PR, make sure your tests are passing, and that the code is properly formatted
         |
         |```
         |sbt prepare
         |
         |sbt test
         |```""".stripMargin
  )
  .enablePlugins(WebsitePlugin)
