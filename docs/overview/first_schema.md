---
id: first_schema
title: "Your first ZIO Schema"
---
## Your First ZIO Schema

ZIO Schema provides macros to help you create `Schema`s out of your data types. But before using the macros,
we should take a look at how to do this the manual way.

### The Domain
Like in [Overview](index.md), we define our example domain like this:

```scala
object Domain {
  final case class Person(name: String, age: Int)

  sealed trait PaymentMethod

  object PaymentMethod {
    final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod
    final case class WireTransfer(accountNumber: String, bankCode: String) extends PaymentMethod
  }

  final case class Customer(person: Person, paymentMethod: PaymentMethod)
  
}
```

### Manual construction of a Schema

This part is similar to other libraries that you might know, e.g. for JSON processing.
Basically, you create a `Schema` for every data type in your domain:

```scala

object ManualConstruction {
  import zio.schema.Schema._
  import Domain._
  import Domain.PaymentMethod._

  val schemaPerson: Schema[Person] = Schema.CaseClass2[String, Int, Person](
    field1 = Schema.Field[String]("name", Schema.primitive[String]),
    field2 = Schema.Field[Int]("age", Schema.primitive[Int]),
    construct = (name, age) => Person(name, age),
    extractField1 = p => p.name,
    extractField2 = p => p.age
  )

  val schemaPaymentMethodWireTransfer: Schema[WireTransfer] = Schema.CaseClass2[String, String, WireTransfer](
    field1 = Schema.Field[String]("accountNumber", Schema.primitive[String]),
    field2 = Schema.Field[String]("bankCode", Schema.primitive[String]),
    construct = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode),
    extractField1 = p => p.accountNumber,
    extractField2 = p => p.bankCode
  )

  val schemaPaymentMethodCreditCard: Schema[CreditCard] = Schema.CaseClass3[String, Int, Int, CreditCard](
    field1 = Schema.Field[String]("number", Schema.primitive[String]),
    field2 = Schema.Field[Int]("expirationMonth", Schema.primitive[Int]),
    field3 = Schema.Field[Int]("expirationYear", Schema.primitive[Int]),
    construct = (number, expirationMonth, expirationYear) => PaymentMethod.CreditCard(number, expirationMonth, expirationYear),
    extractField1 = p => p.number,
    extractField2 = p => p.expirationMonth,
    extractField3 = p => p.expirationYear
  )

  val schemaPaymentMethod: Schema[PaymentMethod] = Schema.Enum2(
    case1 = Case[PaymentMethod.CreditCard, PaymentMethod](
      id = "CreditCard",
      codec = schemaPaymentMethodCreditCard,
      unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.CreditCard]
    ),
    case2 = Case[PaymentMethod.WireTransfer, PaymentMethod](
      id = "WireTransfer",
      codec = schemaPaymentMethodWireTransfer,
      unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.WireTransfer]
    )
  )

  val schemaCustomer: Schema[Customer] = Schema.CaseClass2[Person, PaymentMethod, Customer](
    field1 = Schema.Field[Person]("person", schemaPerson),
    field2 = Schema.Field[PaymentMethod]("paymentMethod", schemaPaymentMethod),
    construct = (person, paymentMethod) => Customer(person, paymentMethod),
    extractField1 = c => c.person,
    extractField2 = c => c.paymentMethod
  )
}

```

### Macro derivation
Using macros, the above code gets reduced to this:

```scala

object MacroConstruction  {
  import Domain._

  val schemaPerson: Schema[Person] = DeriveSchema.gen[Person]

  val schemaPaymentMethod: Schema[PaymentMethod] = DeriveSchema.gen[PaymentMethod]
 
  val schemaCustomer: Schema[Customer] = DeriveSchema.gen[Customer]
}
```

## Applying it to our domain

### Json example
Lets put this all together in a small sample:
```scala
object JsonSample extends zio.App {
  import zio.schema.codec.JsonCodec
  import ManualConstruction._
  import zio.stream.ZStream

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = for {
    _ <- ZIO.unit
    person = Person("Michelle", 32)
    personToJsonTransducer = JsonCodec.encoder[Person](schemaPerson)
    _ <- ZStream(person)
      .transduce(personToJsonTransducer)
      .transduce(ZTransducer.utf8Decode)
      .foreach(ZIO.debug)
  } yield ExitCode.success
}
```

When we run this, we get our expected result printed out:
```json
{"name":"Michelle","age":32}
```

### Protobuf example

```scala
object ProtobufExample extends zio.App {
  import zio.schema.codec.ProtobufCodec
  import ManualConstruction._
  import zio.stream.ZStream

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = for {
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


### Combining different encoders
Let's take a look at a roundtrip converting an object to JSON and back, then converting it to a protobuf and back.
This is a simple example, but it shows how to combine different encoders to achieve a roundtrip.

```scala
object CombiningExample extends zio.App {
  import zio.schema.codec.JsonCodec
  import zio.schema.codec.ProtobufCodec
  import ManualConstruction._
  import zio.stream.ZStream

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = for {
    _ <- ZIO.unit
    _ <- ZIO.debug("combining roundtrip")
    person = Person("Michelle", 32)

    personToJson = JsonCodec.encoder[Person](schemaPerson)
    jsonToPerson = JsonCodec.decoder[Person](schemaPerson)

    personToProto = ProtobufCodec.encoder[Person](schemaPerson)
    protoToPerson = ProtobufCodec.decoder[Person](schemaPerson)

    newPerson <- ZStream(person)
      .tap(v => ZIO.debug("input object is: " + v))
      .transduce(personToJson)
      .transduce(jsonToPerson)
      .tap(v => ZIO.debug("object after json roundtrip: " + v))
      .transduce(personToProto)
      .transduce(protoToPerson)
      .tap(v => ZIO.debug("person after protobuf roundtrip: " + v))
      .runHead
      .some
      .catchAll(error => ZIO.debug(error))
    _ <- ZIO.debug("is old person the new person? " + (person == newPerson).toString)
    _ <- ZIO.debug("old person: " + person)
    _ <- ZIO.debug("new person: " + newPerson)
  } yield ExitCode.success
}
```

