# ZIO-SCHEMA

| Project Stage | CI | Release | Issues |  Discord |
| --- | --- | --- | --- | --- |
| [![Project stage][Stage]][Stage-Page] | ![CI][Badge-CI] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Average time to resolve an issue][badge-iim]][link-iim] | [![badge-discord]][link-discord] |

_ZIO Schema_ is a [ZIO](https://zio.dev)-based library for modeling the schema of data structures as first-class values.

With schema descriptions that can be automatically derived for case classes and sealed traits, _ZIO Schema_ provide powerful features for free:

 - Codecs for any supported protocol (JSON, protobuf, etc.), so data structures can be serialized and deserialized in a principled way
 - Diffing, patching, merging, and other generic-data-based operations
 - Migration of data structures from one schema to another compatible schema
 - Derivation of arbitrary type classes (`Eq`, `Show`, `Ord`, etc.) from the structure of the data

When your data structures need to be serialized, deserialized, persisted, or transported across the wire, then _ZIO Schema_ lets you focus on data modeling and automatically tackle all the low-level, messy details for you.

_ZIO Schema_ is used by a growing number of ZIO libraries, including _ZIO Flow_, _ZIO Redis_, _ZIO Web_, _ZIO SQL_ and _ZIO DynamoDB_.

## Installation

Add in your `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-schema" % "<version>",
  // Required for automatic generic derivation of schemas
  "dev.zio" %% "zio-schema-derivation" % "<version>",
  "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided"
)
```

[Badge-CI]: https://github.com/zio/zio-schema/workflows/CI/badge.svg
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-schema_2.12.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-schema_2.12.svg "Sonatype Snapshots"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-schema_2.12/ "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-schema_2.12/ "Sonatype Snapshots"
[badge-iim]: https://isitmaintained.com/badge/resolution/zio/zio-schema.svg
[link-iim]: https://isitmaintained.com/project/zio/zio-schema
[badge-discord]: https://img.shields.io/discord/630498701860929559?logo=discord "chat ondiscord"
[link-discord]: https://discord.gg/2ccFBr4 "Discord"
[Stage]: https://img.shields.io/badge/Project%20Stage-Development-yellowgreen.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages

## Contributing

For the general guidelines, see ZIO [contributor's guide](https://github.com/zio/zio/blob/master/docs/about/contributing.md).

#### TL;DR

Before you submit a PR, make sure your tests are passing, and that the code is properly formatted

```
sbt prepare

sbt test
```
