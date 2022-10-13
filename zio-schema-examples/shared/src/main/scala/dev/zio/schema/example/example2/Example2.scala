package dev.zio.schema.example.example2

import zio.schema.Schema._
import zio.schema.{ DeriveSchema, Schema, TypeId }
import zio.stream.ZTransducer
import zio.{ Chunk, ExitCode, URIO, ZIO }

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

  val nameType = "name"
  val ageType = "age"
  val numberType = "number"
  val expirationMonthType = "expirationMonth"
  val expirationYearType = "expirationYear"
  val accountNumberType = "accountNumber"
  val bankCodeType = "bankCode"
  val personType = "person"
  val paymentMethodType = "paymentMethod"
 
  object Person {
    val name: Field[nameType.type, String] = Schema.Field[nameType.type, String](nameType, Schema.primitive[String])
    val age: Field[ageType.type, Int]      = Schema.Field[ageType.type, Int](ageType, Schema.primitive[Int])

    val schema: Schema[Person] = Schema.CaseClass2[nameType.type, ageType.type, String, Int, Person](
      TypeId.parse("dev.zio.schema.example.example2.Domain.Person"),
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
      val number: Field[numberType.type, String] = Schema.Field[numberType.type, String](numberType, Schema.primitive[String])

      val expirationMonth: Field[expirationMonthType.type, Int] =
        Schema.Field[expirationMonthType.type, Int](expirationMonthType, Schema.primitive[Int])

      val expirationYear: Field[expirationYearType.type, Int] =
        Schema.Field[expirationYearType.type, Int](expirationYearType, Schema.primitive[Int])
      implicit val schema: Schema[CreditCard] =
        Schema.CaseClass3[numberType.type, expirationMonthType.type, expirationYearType.type, String, Int, Int, CreditCard](
          TypeId.parse("dev.zio.schema.example.example2.Domain.PaymentMethod.CreditCard"),
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

      val accountNumber: Field[accountNumberType.type, String] =
        Schema.Field[accountNumberType.type, String](accountNumberType, Schema.primitive[String])
      val bankCode: Field[bankCodeType.type, String] = Schema.Field[bankCodeType.type, String](bankCodeType, Schema.primitive[String])

      implicit val schema: Schema[WireTransfer] =
        Schema.CaseClass2[accountNumberType.type, bankCodeType.type, String, String, WireTransfer](
          TypeId.parse("dev.zio.schema.example.example2.Domain.PaymentMethod.WireTransfer"),
          field1 = accountNumber,
          field2 = bankCode,
          construct = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode),
          extractField1 = p => p.accountNumber,
          extractField2 = p => p.bankCode
        )
    }

    val schemaPaymentMethod: Schema[PaymentMethod] = Schema.Enum2[CreditCard, WireTransfer, PaymentMethod](
      id = TypeId.parse("dev.zio.schema.example.example2.Domain.PaymentMethod"),
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
    val person: Field[personType.type, Person] = Schema.Field[personType.type, Person](personType, Person.schema)

    val paymentMethod: Field[paymentMethodType.type, PaymentMethod] =
      Schema.Field[paymentMethodType.type, PaymentMethod](paymentMethodType, PaymentMethod.schemaPaymentMethod)

    implicit val schema: Schema[Customer] =
      Schema.CaseClass2[personType.type, paymentMethodType.type, Person, PaymentMethod, Customer](
        TypeId.parse("dev.zio.schema.example.example2.Domain.Customer"),
        field1 = Customer.person,
        field2 = Customer.paymentMethod,
        construct = (person, paymentMethod) => Customer(person, paymentMethod),
        extractField1 = c => c.person,
        extractField2 = c => c.paymentMethod
      )
  }
}

import Domain._

object JsonSample extends zio.App {
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    for {
      _                      <- ZIO.unit
      person                 = Person("Michelle", 32)
      personToJsonTransducer = JsonCodec.encoder[Person](Person.schema)
      _ <- ZStream(person)
            .transduce(personToJsonTransducer)
            .transduce(ZTransducer.utf8Decode)
            .foreach(ZIO.debug)
    } yield ExitCode.success
}

object ProtobufExample extends zio.App {
  import zio.schema.codec.ProtobufCodec
  import zio.stream.ZStream

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("protobuf roundtrip")
      person = Person("Michelle", 32)

      personToProto = ProtobufCodec.encoder[Person](Person.schema)
      protoToPerson = ProtobufCodec.decoder[Person](Person.schema)

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

object CombiningExample extends zio.App {
  import zio.schema.codec.{ JsonCodec, ProtobufCodec }
  import zio.stream.ZStream

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
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
