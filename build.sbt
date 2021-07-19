import sbtcrossproject.CrossPlugin.autoImport.crossProject
import BuildHelper.{ crossProjectSettings, _ }
import explicitdeps.ExplicitDepsPlugin.autoImport.unusedCompileDependenciesFilter
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
    skip in publish := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    zioSchemaJVM,
    zioSchemaJS
  )

lazy val zioSchema = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema"))
  .settings(stdSettings("zio-schema"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"        %% "zio"          % zioVersion,
      "dev.zio"        %% "zio-streams"  % zioVersion,
      "dev.zio"        %% "zio-json"     % zioJsonVersion,
      "com.propensive" %% "magnolia"     % magnoliaVersion,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    )
  )

lazy val zioSchemaJson = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-json"))
  .settings(stdSettings("zio-schema-json"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.json"))
  .dependsOn(zioSchema % "test->test;compile->compile")

lazy val zioSchemaProtobuf = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-schema-protobuf"))
  .settings(stdSettings("zio-schema-protobuf"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.schema.protobuf"))
  .dependsOn(zioSchema % "test->test;compile->compile")

lazy val zioSchemaJS = zioSchema.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSchemaJVM = zioSchema.jvm
lazy val docs = project
  .in(file("zio-schema-docs"))
  .settings(
    skip.in(publish) := true,
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
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(root),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(zioSchemaJVM, zioSchemaProtobuf.jvm)
  .enablePlugins(JmhPlugin)
