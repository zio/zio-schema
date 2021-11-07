
// this is my current version built from zio-schema master .. make your own with sbt publishLocal
// and put the version here.
val zioSchemaVersion  = "0.1.1+16-08f8a3a9+20211107-0110-SNAPSHOT"
val zioOpticsVersion  = "0.1.0"

scalaVersion := "2.13.6"

organization := "com.dominikdorn"
name := "zio-schema-example"

libraryDependencies += "dev.zio" %% "zio-schema" % zioSchemaVersion
libraryDependencies += "dev.zio" %% "zio-schema-optics" % zioSchemaVersion
libraryDependencies += "dev.zio" %% "zio-schema-protobuf" % zioSchemaVersion
libraryDependencies += "dev.zio" %% "zio-schema-json" % zioSchemaVersion
libraryDependencies += "dev.zio" %% "zio-optics" % zioOpticsVersion

