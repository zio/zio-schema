---
id: codecs
title: "Codecs"
---

Once we have our schema, we can combine it with a codec. A codec is a combination of a schema and a serializer. Unlike codecs in other libraries, a codec in ZIO Schema has no type parameter:

```scala
trait Codec {
  def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte]
  def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A]

  def encode[A](schema: Schema[A]): A => Chunk[Byte]
  def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A]
}
```

It basically says:
- `encoder[A]`: Given a `Schema[A]` it is capable of generating an `Encoder[A]` ( `A => Chunk[Byte]`) for any Schema.
- `decoder[A]`: Given a `Schema[A]` it is capable of generating a `Decoder[A]` ( `Chunk[Byte] => Either[String, A]`) for any Schema.

## Binary Codecs

The binary codecs are codecs that can encode/decode a value of some type `A` to/from binary format (e.g. `Chunk[Byte]`).  In ZIO Schema, by having a `BinaryCodec[A]` instance, other than being able to encode/decode a value of type `A` to/from binary format, we can also encode/decode a stream of values of type `A` to/from a stream of binary format.

```scala
import zio.Chunk
import zio.stream.ZPipeline

trait Decoder[Whole, Element, +A] {
  def decode(whole: Whole): Either[DecodeError, A]
  def streamDecoder: ZPipeline[Any, DecodeError, Element, A]
}

trait Encoder[Whole, Element, -A] {
  def encode(value: A): Whole
  def streamEncoder: ZPipeline[Any, Nothing, A, Element]
}

trait Codec[Whole, Element, A] extends Encoder[Whole, Element, A] with Decoder[Whole, Element, A]

trait BinaryCodec[A] extends Codec[Chunk[Byte], Byte, A]
```

To make it simpler, we can think of a `BinaryCodec[A]` as the following trait:

```scala
import zio.Chunk
import zio.stream.ZPipeline

trait BinaryCodec[A] {
  def encode(value: A): Chunk[Byte]
  def decode(whole: Chunk[Byte]): Either[DecodeError, A]

  def streamEncoder: ZPipeline[Any, Nothing, A, Byte]
  def streamDecoder: ZPipeline[Any, DecodeError, Byte, A]
}
```

Example of possible codecs are:

- CSV Codec
- JSON Codec (already available)
- Apache Avro Codec (in progress)
- Apache Thrift Codec (in progress)
- XML Codec
- YAML Codec
- Protobuf Codec (already available)
- QueryString Codec
- etc.

## Avro

To use the Avro codecs, we need to add the following dependency to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-avro" % @VERSION@
```

It has two codecs:

- An **AvroSchemaCodec** to serialize a `Schema[A]` to Avro JSON schema and deserialize an Avro JSON schema to a `Schema.GenericRecord`.
- An **AvroCodec** to serialize/deserialize the Avro binary serialization format.

### AvroSchemaCodec

Here is the definition of the `AvroSchemaCodec`:

```scala
trait AvroSchemaCodec {
  def encode(schema: Schema[_]): scala.util.Either[String, String]
  def decode(bytes: Chunk[Byte]): scala.util.Either[String, Schema[_]]
}
```

The `encode` method takes a `Schema[_]` and returns an `Either[String, String]` where the `Right` side contains the Avro schema in JSON‌ format.

The `decode` method takes a `Chunk[Byte]` which contains the Avro JSON Schema in binary format and returns an `Either[String, Schema[_]]` where the `Right` side contains the ZIO Schema in `GenericRecord` format.

Here is an example of how to use it:

```scala mdoc:compile-only
import zio._
import zio.schema.Schema
import zio.schema.DeriveSchema
import zio.schema.codec.AvroSchemaCodec

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}

object Main extends ZIOAppDefault {
  def run =
    for {
      _          <- ZIO.debug("AvroSchemaCodec Example:")
      avroSchema <- ZIO.fromEither(AvroSchemaCodec.encode(Person.schema))
      _ <- ZIO.debug(s"The person schema in Avro Schema JSON format: $avroSchema")
      avroSchemaBinary = Chunk.fromArray(avroSchema.getBytes)
      zioSchema <- ZIO.fromEither(AvroSchemaCodec.decode(avroSchemaBinary))
      _ <- ZIO.debug(s"The person schema in ZIO Schema GenericRecord format: $zioSchema")
    } yield ()
}
```

The output:

```scala
AvroSchemaCodec Example:
The person schema in Avro Schema JSON format: {"type":"record","name":"Person","fields":[{"name":"name","type":"string"},{"name":"age","type":"int"}]}
The person schema in ZIO Schema GenericRecord format: GenericRecord(Nominal(Chunk(),Chunk(),Person),Field(name,Primitive(string,Chunk())) :*: Field(age,Primitive(int,Chunk())) :*: Empty,Chunk(name(Person)))
```

As we can see, we converted the `Schema[Person]` to Avro schema JSON format, and then we converted it back to the ZIO Schema `GenericRecord` format.

### AvroCodec

We can create a `BinaryCodec[A]` for any type `A` that has a `Schema[A]` instance using `AvroCodec.schemaBasedBinaryCodec`:

```scala
object AvroCodec {
  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] = ???
}
```

Now, let's write an example and see how it works:

```scala mdoc:compile-only
import zio._
import zio.schema.Schema
import zio.schema.DeriveSchema
import zio.schema.codec.{AvroCodec, BinaryCodec}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
  implicit val binaryCodec: BinaryCodec[Person] =
    AvroCodec.schemaBasedBinaryCodec[Person]
}

object Main extends ZIOAppDefault {
  def run =
    for {
      _ <- ZIO.debug("AvroCodec Example:")
      encodedPerson = Person.binaryCodec.encode(Person("John", 42))
      _ <- ZIO.debug(s"encoded person object: ${toHex(encodedPerson)}")
      decodedPerson <- ZIO.fromEither(
        Person.binaryCodec.decode(encodedPerson)
      )
      _ <- ZIO.debug(s"decoded person object: $decodedPerson")
    } yield ()

  def toHex(bytes: Chunk[Byte]): String =
    bytes.map("%02x".format(_)).mkString(" ")
}
```

The output:

```scala
AvroCodec Example:
encoded person object: 08 4a 6f 68 6e 54
decoded person object: Person(John,42)
```

### Annotations

The Apache Avro specification supports some attributes for describing the data which are not part of the default ZIO Schema. To support these extra metadata, we can use annotations defined in the `zio.schema.codec.AvroAnnotations` object.

There tons of annotations that we can use. Let's introduce some of them:

- `name(name: String)`: To change the name of a field or a record.
- `namespace(namespace: String)`: To add the namespace for a field or a record.
- `doc(doc: String)`: To add documentation to a field or a record.
- `aliases(aliases: Set[String])`: To add aliases to a field or a record.
- `avroEnum`: To treat a sealed trait as an Avro enum.
- `scale(scale: Int = 24)` and `precision(precision: Int = 48)`: To describe the scale and precision of a decimal field.
- `decimal(decimalType: DecimalType)`: Used to annotate a `BigInteger` or `BigDecimal` type to indicate the logical type encoding (avro bytes or avro fixed).
- `bytes(bytesType: BytesType)`: Used to annotate a Byte type to indicate the avro type encoding (avro bytes or avro fixed).
- `formatToString`: Used to annotate fields of type `LocalDate`, `LocalTime`, `LocalDateTime` or `Instant` in order to render them as a string using the given formatter instead of rendering them as avro logical types.
- `timeprecision(timeprecisionType: TimePrecisionType)`: Used to indicate the precision (millisecond precision or microsecond precision) of avro logical types `Time`, `Timestamp` and `Local timestamp`
- `error`: Used to annotate a record in order to render it as a avro error record
- `fieldOrder(fieldOrderType: FieldOrderType)`: Used to indicate the avro field order of a record

For example, to change the name of a field in the Avro schema, we can use the `AvroAnnotations.name` annotation:

```scala mdoc:compile-only
import zio.schema.Schema
import zio.schema.DeriveSchema
import zio.schema.codec.AvroAnnotations

@AvroAnnotations.name("User")
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}
```

Now, if we generate the Avro schema for the `Person` class, we will see that the name of the record is `User` instead of `Person`:

```scala
import zio._
import zio.schema.Schema
import zio.schema.DeriveSchema
import zio.schema.codec.AvroSchemaCodec

object Main extends ZIOAppDefault {
  def run =
    for {
      _          <- ZIO.debug("AvroSchemaCodec Example with annotations:")
      avroSchema <- ZIO.fromEither(AvroSchemaCodec.encode(Person.schema))
      _ <- ZIO.debug(s"The person schema in Avro Schema JSON format: $avroSchema")
    } yield ()
}
```

The output:

```scala
The person schema in Avro Schema JSON format: {"type":"record","name":"User","fields":[{"name":"name","type":"string"},{"name":"age","type":{"type":"bytes","logicalType":"decimal","precision":48,"scale":24}}]}
```
