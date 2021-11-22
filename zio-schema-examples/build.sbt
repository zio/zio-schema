scalaVersion := "2.13.7"

organization := "com.dominikdorn"
name := "zio-schema-example"

libraryDependencies ++= {
  val zio              = "dev.zio"
  val zioSchemaVersion = "0.1.2"
  val zioOpticsVersion = "0.1.0"

  Seq(
    zio %% "zio-schema"          % zioSchemaVersion,
    zio %% "zio-schema-optics"   % zioSchemaVersion,
    zio %% "zio-schema-protobuf" % zioSchemaVersion,
    zio %% "zio-schema-json"     % zioSchemaVersion,
    zio %% "zio-optics"          % zioOpticsVersion
  )
}
