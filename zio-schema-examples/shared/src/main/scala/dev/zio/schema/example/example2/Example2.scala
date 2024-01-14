package dev.zio.schema.example.example2

import zio._
import zio.schema.Schema._
import zio.schema.{ Schema, TypeId }
import zio.stream.ZPipeline

/**
 * Example 2 of ZIO-Schema
 *
 * In this example, we pull the definition of our schema into the companion objects of our types.
 * This will help us in the future to avoid having to write the same code over and over again.
 *
 * Again we'll also show that the moved schema can still be used to transform an object from/to JSON and Protobuf.
 */

object Domain {
  sealed trait PaymentMethod

  final case class Person(name: String, age: Int)

  object Person {

    val name: Field[Person, String] =
      Schema.Field[Person, String]("name", Schema.primitive[String], get0 = _.name, set0 = (p, v) => p.copy(name = v))

    val age: Field[Person, Int] =
      Schema.Field[Person, Int]("age", Schema.primitive[Int], get0 = _.age, set0 = (p, v) => p.copy(age = v))

    val schema: Schema[Person] = Schema.CaseClass2[String, Int, Person](
      TypeId.parse("dev.zio.schema.example.example2.Domain.Person"),
      field01 = name,
      field02 = age,
      construct0 = (name, age) => Person(name, age)
    )
  }

  object PaymentMethod {
    final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod

    object CreditCard {

      val number: Field[CreditCard, String] =
        Schema.Field[CreditCard, String](
          "number",
          Schema.primitive[String],
          get0 = _.number,
          set0 = (p, v) => p.copy(number = v)
        )

      val expirationMonth: Field[CreditCard, Int] =
        Schema.Field[CreditCard, Int](
          "expirationMonth",
          Schema.primitive[Int],
          get0 = _.expirationMonth,
          set0 = (p, v) => p.copy(expirationMonth = v)
        )

      val expirationYear: Field[CreditCard, Int] =
        Schema.Field[CreditCard, Int](
          "expirationYear",
          Schema.primitive[Int],
          get0 = _.expirationYear,
          set0 = (p, v) => p.copy(expirationYear = v)
        )
      implicit val schema: Schema[CreditCard] = Schema.CaseClass3[String, Int, Int, CreditCard](
        TypeId.parse("dev.zio.schema.example.example2.Domain.PaymentMethod.CreditCard"),
        field01 = number,
        field02 = expirationMonth,
        field03 = expirationYear,
        construct0 =
          (number, expirationMonth, expirationYear) => PaymentMethod.CreditCard(number, expirationMonth, expirationYear)
      )
    }

    final case class WireTransfer(accountNumber: String, bankCode: String) extends PaymentMethod

    object WireTransfer {

      val accountNumber: Field[WireTransfer, String] =
        Schema.Field[WireTransfer, String](
          "accountNumber",
          Schema.primitive[String],
          get0 = _.accountNumber,
          set0 = (p, v) => p.copy(accountNumber = v)
        )

      val bankCode: Field[WireTransfer, String] =
        Schema.Field[WireTransfer, String](
          "bankCode",
          Schema.primitive[String],
          get0 = _.bankCode,
          set0 = (p, v) => p.copy(bankCode = v)
        )

      implicit val schema: Schema[WireTransfer] = Schema.CaseClass2[String, String, WireTransfer](
        TypeId.parse("dev.zio.schema.example.example2.Domain.PaymentMethod.WireTransfer"),
        field01 = accountNumber,
        field02 = bankCode,
        construct0 = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode)
      )
    }

    val schemaPaymentMethod: Schema[PaymentMethod] = Schema.Enum2[CreditCard, WireTransfer, PaymentMethod](
      id = TypeId.parse("dev.zio.schema.example.example2.Domain.PaymentMethod"),
      case1 = Case[PaymentMethod, CreditCard](
        id = "CreditCard",
        schema = CreditCard.schema,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.CreditCard],
        construct = pc => pc.asInstanceOf[PaymentMethod],
        isCase = _.isInstanceOf[PaymentMethod.CreditCard],
        annotations = Chunk.empty
      ),
      case2 = Case[PaymentMethod, WireTransfer](
        id = "WireTransfer",
        schema = WireTransfer.schema,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.WireTransfer],
        construct = pc => pc.asInstanceOf[PaymentMethod],
        isCase = _.isInstanceOf[PaymentMethod.WireTransfer],
        annotations = Chunk.empty
      ),
      annotations = Chunk.empty
    )

  }

  final case class Customer(person: Person, paymentMethod: PaymentMethod)

  object Customer {

    val person: Field[Customer, Person] =
      Schema.Field[Customer, Person]("person", Person.schema, get0 = _.person, set0 = (p, v) => p.copy(person = v))

    val paymentMethod: Field[Customer, PaymentMethod] =
      Schema.Field[Customer, PaymentMethod](
        "paymentMethod",
        PaymentMethod.schemaPaymentMethod,
        get0 = _.paymentMethod,
        set0 = (p, v) => p.copy(paymentMethod = v)
      )

    implicit val schema: Schema[Customer] = Schema.CaseClass2[Person, PaymentMethod, Customer](
      TypeId.parse("dev.zio.schema.example.example2.Domain.Customer"),
      field01 = Customer.person,
      field02 = Customer.paymentMethod,
      construct0 = (person, paymentMethod) => Customer(person, paymentMethod)
    )
  }
}

import dev.zio.schema.example.example2.Domain._

object JsonSample extends zio.ZIOAppDefault {
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      person = Person("Michelle", 32)
      personToJsonPipeline = JsonCodec
        .schemaBasedBinaryCodec[Person](Person.schema)
        .streamEncoder
      _ <- ZStream(person)
            .via(personToJsonPipeline)
            .via(ZPipeline.utf8Decode)
            .foreach(f => ZIO.debug(f))
    } yield ExitCode.success
}

object ProtobufExample extends ZIOAppDefault {
  import zio.schema.codec.ProtobufCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("protobuf roundtrip")
      person = Person("Michelle", 32)

      personToProto = ProtobufCodec.protobufCodec[Person](Person.schema).streamEncoder
      protoToPerson = ProtobufCodec.protobufCodec[Person](Person.schema).streamDecoder

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

object CombiningExample extends zio.ZIOAppDefault {
  import zio.schema.codec.{ ProtobufCodec }
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("combining roundtrip")
      person = Person("Michelle", 32)

      personToJson = JsonCodec.schemaBasedBinaryCodec[Person](Person.schema).streamEncoder
      jsonToPerson = JsonCodec.schemaBasedBinaryCodec[Person](Person.schema).streamDecoder

      personToProto = ProtobufCodec.protobufCodec[Person](Person.schema).streamEncoder
      protoToPerson = ProtobufCodec.protobufCodec[Person](Person.schema).streamDecoder

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
