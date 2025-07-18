import sbtcrossproject.CrossPlugin.autoImport._
import BuildHelper.{ crossProjectSettings, _ }
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts

Global / onChangedBuildSource := ReloadOnSourceChanges

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

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots".at(centralSnapshots))
  else localStaging.value
}
ThisBuild / evictionErrorLevel := Level.Warn

scalacOptions ++= Seq("-scalajs")

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll;fix")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("fix", "scalafixAll")
addCommandAlias("fixCheck", "scalafixAll --check")

addCommandAlias(
  "testJVM",
  "testsJVM/test; zioSchemaMacrosJVM/test; zioSchemaJVM/test; zioSchemaDerivationJVM/test;" +
    "zioSchemaOpticsJVM/test; zioSchemaJsonJVM/test; zioSchemaProtobufJVM/test; zioSchemaZioTestJVM/test;" +
    "zioSchemaAvro/test; zioSchemaThrift/test; zioSchemaBson/test; zioSchemaMsgPack/test"
)

addCommandAlias(
  "testNative",
  "zioSchemaMacrosNative/test; zioSchemaDerivationNative/test; zioSchemaJsonNative/test; zioSchemaOpticsNative/test;" +
    "testsNative/test; zioSchemaNative/test; zioSchemaZioTestNative/test; zioSchemaProtobufNative/test;"
)

addCommandAlias(
  "testJS",
  "zioSchemaMacrosJS/test; zioSchemaDerivationJS/test; zioSchemaJsonJS/test; zioSchemaOpticsJS/test; testsJS/test; zioSchemaJS/test; " +
    "zioSchemaZioTestJS/test; zioSchemaProtobufJS/test;"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "zio-schema",
    publish / skip := true,
    mimaPreviousArtifacts := Set()
//    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    zioSchemaMacrosJVM,
    zioSchemaMacrosJS,
    zioSchemaMacros.native,
    zioSchemaJVM,
    zioSchemaJS,
    zioSchema.native,
    zioSchemaDerivationJVM,
    zioSchemaDerivationJS,
    zioSchemaDerivation.native,
    zioSchemaJsonJVM,
    zioSchemaJsonJS,
    zioSchemaJson.native,
    zioSchemaOpticsJS,
    zioSchemaOpticsJVM,
    zioSchemaOptics.native,
    zioSchemaProtobufJS,
    zioSchemaProtobufJVM,
    zioSchemaProtobuf.native,
    zioSchemaExamplesJS,
    zioSchemaExamplesJVM,
    zioSchemaExamples.native,
    testsJVM,
    testsJS,
    tests.native,
    zioSchemaZioTestJVM,
    zioSchemaZioTestJS,
    zioSchemaZioTest.native,
    zioSchemaThrift,
    zioSchemaAvro,
    zioSchemaBson,
    zioSchemaMsgPack,
    docs
  )

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("tests"))
  .dependsOn(zioSchemaDerivation % "compile->test", zioSchema % "test->test", zioSchemaZioTest % "compile->test")
  .settings(stdSettings("zio-schema-tests"))
  .settings(
    publish / skip := true,
    mimaPreviousArtifacts := Set()
  )
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(testDeps)

lazy val testsJS = tests.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val testsJVM = tests.jvm

lazy val zioSchemaMacros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-macros"))
  .settings(stdSettings("zio-schema-macros"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(macroDefinitionSettings)
  .nativeSettings(
    Test / fork := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .settings(testDeps)

lazy val zioSchemaMacrosJS  = zioSchemaMacros.js
lazy val zioSchemaMacrosJVM = zioSchemaMacros.jvm

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
  .nativeSettings(
    Test / fork := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .settings(testDeps)
  .dependsOn(zioSchemaMacros)

lazy val zioSchemaJS = zioSchema.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaJVM = zioSchema.jvm

lazy val zioSchemaDerivation = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-derivation"))
  .dependsOn(zioSchema, zioSchema % "test->test")
  .settings(stdSettings("zio-schema-derivation"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"         % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion,
      "dev.zio" %%% "zio-prelude" % zioPreludeVersion
    )
  )
  .jvmSettings(
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
  .jsSettings(
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
  .nativeSettings(
    Test / fork := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .settings(testDeps)

lazy val zioSchemaDerivationJS = zioSchemaDerivation.js

lazy val zioSchemaDerivationJVM = zioSchemaDerivation.jvm

lazy val zioSchemaJson = crossProject(JSPlatform, JVMPlatform, NativePlatform)
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
  .nativeSettings(
    Test / fork := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .jsSettings(scalaJSLinkerConfig ~= { _.withOptimizer(false) })
  .settings(testDeps)

lazy val zioSchemaJsonJS = zioSchemaJson.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaJsonJVM = zioSchemaJson.jvm

lazy val zioSchemaProtobuf = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-protobuf"))
  .dependsOn(zioSchema, zioSchemaDerivation, tests % "test->test")
  .settings(stdSettings("zio-schema-protobuf"))
  .settings(dottySettings)
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.protobuf"))
  .nativeSettings(
    Test / fork := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .settings(testDeps)

lazy val zioSchemaProtobufJS = zioSchemaProtobuf.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaProtobufJVM = zioSchemaProtobuf.jvm

lazy val zioSchemaThrift = project
  .in(file("zio-schema-thrift"))
  .dependsOn(zioSchema.jvm, zioSchemaDerivation.jvm, tests.jvm % "test->test")
  .settings(stdSettings("zio-schema-thrift"))
  .settings(dottySettings)
  .settings(buildInfoSettings("zio.schema.thrift"))
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.thrift"  % "libthrift"              % thriftVersion,
      "jakarta.annotation" % "jakarta.annotation-api" % javaxAnnotationApiVersion
    )
  )
  .settings(testDeps)

lazy val zioSchemaMsgPack = project
  .in(file("zio-schema-msg-pack"))
  .dependsOn(zioSchema.jvm, zioSchemaDerivation.jvm, tests.jvm % "test->test")
  .settings(stdSettings("zio-schema-msg-pack"))
  .settings(dottySettings)
  .settings(buildInfoSettings("zio.schema.msgpack"))
  .settings(
    libraryDependencies ++= Seq(
      "org.msgpack"                  % "msgpack-core"               % msgpackVersion,
      "org.msgpack"                  % "jackson-dataformat-msgpack" % msgpackVersion % Test,
      "com.fasterxml.jackson.module" %% "jackson-module-scala"      % jacksonScalaVersion % Test
    )
  )
  .settings(testDeps)

lazy val zioSchemaAvro = project
  .in(file("zio-schema-avro"))
  .dependsOn(zioSchema.jvm, zioSchemaDerivation.jvm, tests.jvm % "test->test")
  .settings(stdSettings("zio-schema-avro"))
  .settings(dottySettings)
  .settings(buildInfoSettings("zio.schema.avro"))
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.avro"        % "avro"                     % avroVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion
    )
  )
  .settings(testDeps)

lazy val zioSchemaBson = project
  .in(file("zio-schema-bson"))
  .dependsOn(zioSchema.jvm, zioSchemaDerivation.jvm, zioSchemaZioTest.jvm % Test, tests.jvm % "test->test")
  .settings(stdSettings("zio-schema-bson"))
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
  .settings(testDeps)

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
  .nativeSettings(
    Test / fork := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .settings(testDeps)

lazy val zioSchemaOpticsJS = zioSchemaOptics.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaOpticsJVM = zioSchemaOptics.jvm

lazy val zioSchemaExamples = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-schema-examples"))
  .settings(stdSettings("zio-schema-examples"))
  .settings(crossScalaVersions -= Scala212)
  .dependsOn(zioSchema, zioSchemaJson, zioSchemaProtobuf, zioSchemaOptics)
  .settings(
    publish / skip := true,
    mimaPreviousArtifacts := Set(),
    moduleName := "zio-schema-example",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings"
  )
  .nativeSettings(
    Test / fork := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .settings(testDeps)

lazy val zioSchemaExamplesJS = zioSchemaExamples.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaExamplesJVM = zioSchemaExamples.jvm

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
  .settings(testDeps)

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
    mdoc := {
      (Compile / run).evaluated
      mdoc.evaluated
    },
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
  .settings(testDeps)
  .dependsOn(
    zioSchemaJVM,
    zioSchemaProtobufJVM,
    zioSchemaJsonJVM,
    zioSchemaOpticsJVM,
    zioSchemaAvro,
    zioSchemaBson,
    zioSchemaMsgPack,
    zioSchemaThrift
  )
  .enablePlugins(WebsitePlugin)

lazy val testDeps = Seq(
  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio-test"     % zioVersion % Test,
    "dev.zio" %%% "zio-test-sbt" % zioVersion % Test
  )
)
