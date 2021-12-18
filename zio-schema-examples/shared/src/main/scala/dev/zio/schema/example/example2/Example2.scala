package dev.zio.schema.example.example2

import zio.schema.Schema
import zio.schema.Schema._
import zio.stream.ZPipeline
import zio.{ Chunk, ExitCode, ZIO }
import zio.{ ZEnv, ZIOAppArgs }

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
    val name: Field[String] = Schema.Field[String]("name", Schema.primitive[String])
    val age: Field[Int]     = Schema.Field[Int]("age", Schema.primitive[Int])

    val schema: Schema[Person] = Schema.CaseClass2[String, Int, Person](
      field1 = name,
      field2 = age,
      construct = (name, age) => Person(name, age),
      extractField1 = p => p.name,
      extractField2 = p => p.age
    )
  }

  object PaymentMethod {
    final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod

    object CreditCard {
      val number: Field[String]       = Schema.Field[String]("number", Schema.primitive[String])
      val expirationMonth: Field[Int] = Schema.Field[Int]("expirationMonth", Schema.primitive[Int])
      val expirationYear: Field[Int]  = Schema.Field[Int]("expirationYear", Schema.primitive[Int])
      implicit val schema: Schema[CreditCard] = Schema.CaseClass3[String, Int, Int, CreditCard](
        field1 = number,
        field2 = expirationMonth,
        field3 = expirationYear,
        construct = (number, expirationMonth, expirationYear) =>
          PaymentMethod.CreditCard(number, expirationMonth, expirationYear),
        extractField1 = p => p.number,
        extractField2 = p => p.expirationMonth,
        extractField3 = p => p.expirationYear
      )
    }

    final case class WireTransfer(accountNumber: String, bankCode: String) extends PaymentMethod

    object WireTransfer {
      val accountNumber: Field[String] = Schema.Field[String]("accountNumber", Schema.primitive[String])
      val bankCode: Field[String]      = Schema.Field[String]("bankCode", Schema.primitive[String])

      implicit val schema: Schema[WireTransfer] = Schema.CaseClass2[String, String, WireTransfer](
        field1 = accountNumber,
        field2 = bankCode,
        construct = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode),
        extractField1 = p => p.accountNumber,
        extractField2 = p => p.bankCode
      )
    }

    val schemaPaymentMethod: Schema[PaymentMethod] = Schema.Enum2[CreditCard, WireTransfer, PaymentMethod](
      case1 = Case[CreditCard, PaymentMethod](
        id = "CreditCard",
        codec = CreditCard.schema,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.CreditCard],
        annotations = Chunk.empty
      ),
      case2 = Case[WireTransfer, PaymentMethod](
        id = "WireTransfer",
        codec = WireTransfer.schema,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.WireTransfer],
        annotations = Chunk.empty
      ),
      annotations = Chunk.empty
    )

  }

  final case class Customer(person: Person, paymentMethod: PaymentMethod)

  object Customer {
    val person: Field[Person] = Schema.Field[Person]("person", Person.schema)

    val paymentMethod: Field[PaymentMethod] =
      Schema.Field[PaymentMethod]("paymentMethod", PaymentMethod.schemaPaymentMethod)

    implicit val schema: Schema[Customer] = Schema.CaseClass2[Person, PaymentMethod, Customer](
      field1 = Customer.person,
      field2 = Customer.paymentMethod,
      construct = (person, paymentMethod) => Customer(person, paymentMethod),
      extractField1 = c => c.person,
      extractField2 = c => c.paymentMethod
    )
  }
}

import dev.zio.schema.example.example2.Domain._

object JsonSample extends zio.ZIOAppDefault {
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    for {
      _                    <- ZIO.unit
      person               = Person("Michelle", 32)
      personToJsonPipeline = JsonCodec.encoder[Person](Person.schema)
      _ <- ZStream(person)
            .via(personToJsonPipeline)
            .via(ZPipeline.utf8Decode)
            .foreach(f => ZIO.debug(f))
    } yield ExitCode.success
}

object ProtobufExample extends zio.ZIOAppDefault {
  import zio.schema.codec.ProtobufCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("protobuf roundtrip")
      person = Person("Michelle", 32)

      personToProto = ProtobufCodec.encoder[Person](Person.schema)
      protoToPerson = ProtobufCodec.decoder[Person](Person.schema)

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
  import zio.schema.codec.{ JsonCodec, ProtobufCodec }
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("combining roundtrip")
      person = Person("Michelle", 32)

      personToJson = JsonCodec.encoder[Person](Person.schema)
      jsonToPerson = JsonCodec.decoder[Person](Person.schema)

      personToProto = ProtobufCodec.encoder[Person](Person.schema)
      protoToPerson = ProtobufCodec.decoder[Person](Person.schema)

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
