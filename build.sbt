import sbtcrossproject.CrossPlugin.autoImport.crossProject
import BuildHelper.{ crossProjectSettings, _ }
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSUseMainModuleInitializer

inThisBuild(
  List(
    name := "zio-schema",
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
    zioSchemaMacrosJVM,
    zioSchemaMacrosJS,
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
    zioSchemaZioTestJS,
    zioSchemaThriftJS,
    zioSchemaThriftJVM,
    zioSchemaAvroJS,
    zioSchemaAvroJVM,
    zioSchemaBsonJVM,
    zioSchemaMsgPackJS,
    zioSchemaMsgPackJVM,
    docs
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

lazy val zioSchemaMacros = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-macros"))
  .settings(stdSettings("zio-schema-macros"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(macroDefinitionSettings)

lazy val zioSchemaMacrosJS  = zioSchemaMacros.js
lazy val zioSchemaMacrosJVM = zioSchemaMacros.jvm

lazy val zioSchema = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema"))
  .settings(stdSettings("zio-schema"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"                % zioVersion,
      "dev.zio" %% "zio-streams"        % zioVersion,
      "dev.zio" %% "zio-prelude"        % zioPreludeVersion,
      "dev.zio" %% "zio-constraintless" % zioConstraintlessVersion
    )
  )
  .dependsOn(zioSchemaMacros)

lazy val zioSchemaJS = zioSchema.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaJVM = zioSchema.jvm

lazy val zioSchemaDerivation = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-derivation"))
  .dependsOn(zioSchema, zioSchema % "test->test")
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

lazy val zioSchemaProtobuf = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-protobuf"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-protobuf"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.protobuf"))

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

lazy val zioSchemaMsgPackJS = zioSchemaMsgPack.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaMsgPackJVM = zioSchemaMsgPack.jvm

lazy val zioSchemaAvro = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-avro"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-avro"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.avro"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"         %% "zio-json" % zioJsonVersion,
      "org.apache.avro" % "avro"      % avroVersion
    )
  )

lazy val zioSchemaAvroJS = zioSchemaAvro.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaAvroJVM = zioSchemaAvro.jvm

lazy val zioSchemaBson = crossProject(JVMPlatform)
  .in(file("zio-schema-bson"))
  .dependsOn(zioSchema, zioSchemaDerivation, zioSchemaZioTest % Test, tests % "test->test")
  .settings(stdSettings("zio-schema-bson"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.bson"))
  .settings(
    libraryDependencies ++= Seq(
      "org.mongodb"            % "bson"                     % bsonVersion,
      "dev.zio"                %% "zio-bson"                % zioBsonVersion,
      "dev.zio"                %% "zio"                     % zioVersion, // zio.Chunk
      "dev.zio"                %% "zio-test-magnolia"       % zioVersion % Test, // TODO: implement DeriveDiff in zioSchemaZioTest
      "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion
    ),
    scalacOptions -= "-Xfatal-warnings" // cross-version imports
  )

lazy val zioSchemaBsonJVM = zioSchemaBson.jvm

lazy val zioSchemaOptics = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-optics"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-optics"))
  .settings(dottySettings)
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

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(zioSchemaJVM, zioSchemaDerivationJVM, zioSchemaProtobufJVM)
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
    mainModuleName := (zioSchemaJVM / moduleName).value,
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
  .dependsOn(zioSchemaJVM, zioSchemaProtobufJVM)
  .enablePlugins(WebsitePlugin)
