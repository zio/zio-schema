package dev.zio.schema.example.example1

import zio._
import zio.schema.{ DeriveSchema, Schema, TypeId }
import zio.stream.ZPipeline

/**
 * Example 1 of ZIO-Schema:
 *
 * In this example we define our basic Domain Model.
 * Then we'll show how to manually construct a Schema for the given domain model
 * and how to derive one using macros.
 *
 * We'll then use the Schema to transform instances of our classes (Person) to/from JSON and Protobuf.
 */

object Domain {
  sealed trait PaymentMethod

  final case class Person(name: String, age: Int)

  final case class Customer(person: Person, paymentMethod: PaymentMethod)

  object PaymentMethod {
    final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod
    final case class WireTransfer(accountNumber: String, bankCode: String)                 extends PaymentMethod
  }
}

import dev.zio.schema.example.example1.Domain._

object ManualConstruction {
  import Domain.PaymentMethod._
  import zio.schema.Schema._

  val schemaPerson: Schema[Person] = Schema.CaseClass2[String, Int, Person](
    TypeId.parse("dev.zio.schema.example.example1.Domain.Person"),
    field1 = Schema.Field[Person, String]("name", Schema.primitive[String], get = _.name),
    field2 = Schema.Field[Person, Int]("age", Schema.primitive[Int], get = _.age),
    construct = (name, age) => Person(name, age)
  )

  val schemaPaymentMethodWireTransfer: Schema[WireTransfer] = Schema.CaseClass2[String, String, WireTransfer](
    TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.WireTransfer"),
    field1 = Schema.Field[WireTransfer, String]("accountNumber", Schema.primitive[String], get = _.accountNumber),
    field2 = Schema.Field[WireTransfer, String]("bankCode", Schema.primitive[String], get = _.bankCode),
    construct = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode)
  )

  val schemaPaymentMethodCreditCard: Schema[CreditCard] = Schema.CaseClass3[String, Int, Int, CreditCard](
    TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.CreditCard"),
    field1 = Schema.Field[CreditCard, String]("number", Schema.primitive[String], get = _.number),
    field2 = Schema.Field[CreditCard, Int]("expirationMonth", Schema.primitive[Int], get = _.expirationMonth),
    field3 = Schema.Field[CreditCard, Int]("expirationYear", Schema.primitive[Int], get = _.expirationYear),
    construct =
      (number, expirationMonth, expirationYear) => PaymentMethod.CreditCard(number, expirationMonth, expirationYear)
  )

  val schemaPaymentMethod: Schema[PaymentMethod] =
    Schema.Enum2[PaymentMethod.CreditCard, PaymentMethod.WireTransfer, PaymentMethod](
      id = TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod"),
      case1 = Case[PaymentMethod.CreditCard, PaymentMethod](
        id = "CreditCard",
        schema = schemaPaymentMethodCreditCard,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.CreditCard],
        annotations = Chunk.empty
      ),
      case2 = Case[PaymentMethod.WireTransfer, PaymentMethod](
        id = "WireTransfer",
        schema = schemaPaymentMethodWireTransfer,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.WireTransfer],
        annotations = Chunk.empty
      ),
      annotations = Chunk.empty
    )

  val schemaCustomer: Schema[Customer] = Schema.CaseClass2[Person, PaymentMethod, Customer](
    TypeId.parse("dev.zio.schema.example.example1.Domain.Customer"),
    field1 = Schema.Field[Customer, Person]("person", schemaPerson, get = _.person),
    field2 = Schema.Field[Customer, PaymentMethod]("paymentMethod", schemaPaymentMethod, get = _.paymentMethod),
    construct = (person, paymentMethod) => Customer(person, paymentMethod)
  )

}

object MacroConstruction {

  val schemaPerson: Schema[Person] = DeriveSchema.gen[Person]

  val schemaPaymentMethod: Schema[PaymentMethod] = DeriveSchema.gen[PaymentMethod]

  val schemaCustomer: Schema[Customer] = DeriveSchema.gen[Customer]

}

object JsonSample extends zio.ZIOAppDefault {
  import ManualConstruction._
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _                      <- ZIO.unit
      person                 = Person("Michelle", 32)
      personToJsonTransducer = JsonCodec.encoder[Person](schemaPerson)
      _ <- ZStream(person)
            .via(personToJsonTransducer)
            .via(ZPipeline.utf8Decode)
            .foreach(ZIO.debug(_))
    } yield ExitCode.success
}

object ProtobufExample extends ZIOAppDefault {
  import ManualConstruction._
  import zio.schema.codec.ProtobufCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("protobuf roundtrip")
      person = Person("Michelle", 32)

      personToProto = ProtobufCodec.encoder[Person](schemaPerson)
      protoToPerson = ProtobufCodec.decoder[Person](schemaPerson)

      newPerson <- ZStream(person)
                    .via(personToProto)
                    .via(protoToPerson)
                    .runHead
                    .some
                    .catchAll(error => ZIO.debug(error))
      _ <- ZIO.debug("is old person the new person? " + (person == newPerson).toString)
      _ <- ZIO.debug("old person: " + person)
      _ <- ZIO.debug("new person: " + newPerson)
    } yield ExitCode.success
}

object CombiningExample extends ZIOAppDefault {
  import ManualConstruction._
  import zio.schema.codec.{ JsonCodec, ProtobufCodec }
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("combining roundtrip")
      person = Person("Michelle", 32)

      personToJson = JsonCodec.encoder[Person](schemaPerson)
      jsonToPerson = JsonCodec.decoder[Person](schemaPerson)

      personToProto = ProtobufCodec.encoder[Person](schemaPerson)
      protoToPerson = ProtobufCodec.decoder[Person](schemaPerson)

      newPerson <- ZStream(person)
                    .tap(v => ZIO.debug("input object is: " + v))
                    .via(personToJson)
                    .via(jsonToPerson)
                    .tap(v => ZIO.debug("object after json roundtrip: " + v))
                    .via(personToProto)
                    .via(protoToPerson)
                    .tap(v => ZIO.debug("person after protobuf roundtrip: " + v))
                    .runHead
                    .some
                    .catchAll(error => ZIO.debug(error))
      _ <- ZIO.debug("is old person the new person? " + (person == newPerson).toString)
      _ <- ZIO.debug("old person: " + person)
      _ <- ZIO.debug("new person: " + newPerson)
    } yield ExitCode.success
}
