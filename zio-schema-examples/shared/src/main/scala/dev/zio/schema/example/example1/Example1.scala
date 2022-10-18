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

  val name            = "name"
  val age             = "age"
  val accountNumber   = "accountNumber"
  val bankCode        = "bankCode"
  val number          = "number"
  val expirationMonth = "expirationMonth"
  val expirationYear  = "expirationYear"
  val person          = "person"
  val paymentMethod   = "paymentMethod"

  val schemaPerson: Schema[Person] = Schema.CaseClass2[name.type, age.type, String, Int, Person](
    TypeId.parse("dev.zio.schema.example.example1.Domain.Person"),
    field1 = Schema.Field[name.type, String](name, Schema.primitive[String]),
    field2 = Schema.Field[age.type, Int](age, Schema.primitive[Int]),
    construct = (name, age) => Person(name, age),
    extractField1 = p => p.name,
    extractField2 = p => p.age
  )

  val schemaPaymentMethodWireTransfer: Schema[WireTransfer] =
    Schema.CaseClass2[accountNumber.type, bankCode.type, String, String, WireTransfer](
      TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.WireTransfer"),
      field1 = Schema.Field[accountNumber.type, String](accountNumber, Schema.primitive[String]),
      field2 = Schema.Field[bankCode.type, String](bankCode, Schema.primitive[String]),
      construct = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode),
      extractField1 = p => p.accountNumber,
      extractField2 = p => p.bankCode
    )

  val schemaPaymentMethodCreditCard: Schema[CreditCard] =
    Schema.CaseClass3[number.type, expirationMonth.type, expirationYear.type, String, Int, Int, CreditCard](
      TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.CreditCard"),
      field1 = Schema.Field[number.type, String](number, Schema.primitive[String]),
      field2 = Schema.Field[expirationMonth.type, Int](expirationMonth, Schema.primitive[Int]),
      field3 = Schema.Field[expirationYear.type, Int](expirationYear, Schema.primitive[Int]),
      construct =
        (number, expirationMonth, expirationYear) => PaymentMethod.CreditCard(number, expirationMonth, expirationYear),
      extractField1 = p => p.number,
      extractField2 = p => p.expirationMonth,
      extractField3 = p => p.expirationYear
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

  val schemaCustomer: Schema[Customer] =
    Schema.CaseClass2[person.type, paymentMethod.type, Person, PaymentMethod, Customer](
      TypeId.parse("dev.zio.schema.example.example1.Domain.Customer"),
      field1 = Schema.Field[person.type, Person](person, schemaPerson),
      field2 = Schema.Field[paymentMethod.type, PaymentMethod](paymentMethod, schemaPaymentMethod),
      construct = (person, paymentMethod) => Customer(person, paymentMethod),
      extractField1 = c => c.person,
      extractField2 = c => c.paymentMethod
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
