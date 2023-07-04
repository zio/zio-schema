---
id: protobuf-example
title: "Protobuf Example"
---

```scala
object ProtobufExample extends zio.App {
  import zio.schema.codec.ProtobufCodec
  import ManualConstruction._
  import zio.stream.ZStream

  override def run(args: List[String]): UIO[ExitCode] = for {
    _ <- ZIO.unit
    _ <- ZIO.debug("protobuf roundtrip")
    person = Person("Michelle", 32)

    personToProto = ProtobufCodec.encoder[Person](schemaPerson)
    protoToPerson = ProtobufCodec.decoder[Person](schemaPerson)

    newPerson <- ZStream(person)
      .transduce(personToProto)
      .transduce(protoToPerson)
      .runHead
      .some
      .catchAll(error => ZIO.debug(error))
    _ <- ZIO.debug("is old person the new person? " + (person == newPerson).toString)
    _ <- ZIO.debug("old person: " + person)
    _ <- ZIO.debug("new person: " + newPerson)
  } yield ExitCode.success
}
```
