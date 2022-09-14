package dev.zio.schema.example.example2

import zio._
import zio.schema.Schema._
import zio.schema.{ DeriveSchema, Schema }
import zio.stream.ZTransducer
import zio.{ Chunk, ExitCode, URIO, ZIO }
import zio.schema.diff.matching.MatchTree

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
      val number: Field[String]       = Schema.Field[String]("number", Schema.primitive[String])
      val expirationMonth: Field[Int] = Schema.Field[Int]("expirationMonth", Schema.primitive[Int])
      val expirationYear: Field[Int]  = Schema.Field[Int]("expirationYear", Schema.primitive[Int])
      implicit val schema: Schema[CreditCard] = Schema.CaseClass3[String, Int, Int, CreditCard](
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
      val accountNumber: Field[String] = Schema.Field[String]("accountNumber", Schema.primitive[String])
      val bankCode: Field[String]      = Schema.Field[String]("bankCode", Schema.primitive[String])

      implicit val schema: Schema[WireTransfer] = Schema.CaseClass2[String, String, WireTransfer](
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
    val person: Field[Person] = Schema.Field[Person]("person", Person.schema)

    val paymentMethod: Field[PaymentMethod] =
      Schema.Field[PaymentMethod]("paymentMethod", PaymentMethod.schemaPaymentMethod)

    implicit val schema: Schema[Customer] = Schema.CaseClass2[Person, PaymentMethod, Customer](
      TypeId.parse("dev.zio.schema.example.example2.Domain.Customer"),
      field1 = Customer.person,
      field2 = Customer.paymentMethod,
      construct = (person, paymentMethod) => Customer(person, paymentMethod),
      extractField1 = c => c.person,
      extractField2 = c => c.paymentMethod
    )
  }
}

import dev.zio.schema.example.example2.Domain._

object magic extends App {
  val person        = Person("Michelle", 32)
  val paymentMethod = PaymentMethod.CreditCard("123456789", 1, 2020)
  val customer      = Customer(person, paymentMethod)
  println(customer)
  println("---------------------------------------------------")
  val uglyThing     = MatchTree.fromValue(customer)
  println("---------------------------------------------------")
  println(uglyThing)
  println("---------------------------------------------------")
  println(uglyThing.leaves.mkString("\n"))

  //schema
  // CaseClass2(
  //   Field("person", CaseClass2(Field("name", Primitive("string", Chunk())), Field("age", Primitive("int", Chunk())))),
  //   Field(
  //     "paymentMethod",
  //     Enum2(
  //       Case(
  //         "CreditCard",
  //         CaseClass3(
  //           Field("number", Primitive("string", Chunk())),
  //           Field("expirationMonth", Primitive("int", Chunk())),
  //           Field("expirationYear", Primitive("int", Chunk()))
  //         ),
  //         Chunk()
  //       ),
  //       Case(
  //         "WireTransfer",
  //         CaseClass2(
  //           Field("accountNumber", Primitive("string", Chunk())),
  //           Field("bankCode", Primitive("string", Chunk()))
  //         ),
  //         Chunk()
  //       ),
  //       Chunk()
  //     )
  //   )
  // )

  // Record(
  //   ListMap(
  //     "person" -> Record(ListMap("name" -> Primitive(Michelle, "string"), "age" -> Primitive(32, "int"))),
  //     "paymentMethod" -> Enumeration(
  //       (
  //         "CreditCard",
  //         Record(
  //           ListMap(
  //             "number"          -> Primitive("123456789", "string"),
  //             "expirationMonth" -> Primitive(1, "int"),
  //             "expirationYear"  -> Primitive(2020, "int")
  //           )
  //         )
  //       )
  //     )
  //   )
  // )
  // Product(
  //   Chunk(),
  //   Chunk(
  //     (
  //       person,
  //       Product(
  //         Chunk(person),
  //         Chunk((name, Value(string, Chunk(person, name), false)), (age, Value(int, Chunk(person, age), false))),
  //         false
  //       )
  //     ),
  //     (
  //       paymentMethod,
  //       Sum(
  //         Chunk(paymentMethod),
  //         Chunk(
  //           (
  //             CreditCard,
  //             Product(
  //               Chunk(paymentMethod, CreditCard),
  //               Chunk(
  //                 (number, Value(string, Chunk(paymentMethod, CreditCard, number), false)),
  //                 (expirationMonth, Value(int, Chunk(paymentMethod, CreditCard, expirationMonth), false)),
  //                 (expirationYear, Value(int, Chunk(paymentMethod, CreditCard, expirationYear), false))
  //               ),
  //               false
  //             )
  //           ),
  //           (
  //             WireTransfer,
  //             Product(
  //               Chunk(paymentMethod, WireTransfer),
  //               Chunk(
  //                 (accountNumber, Value(string, Chunk(paymentMethod, WireTransfer, accountNumber), false)),
  //                 (bankCode, Value(string, Chunk(paymentMethod, WireTransfer, bankCode), false))
  //               ),
  //               false
  //             )
  //           )
  //         ),
  //         false
  //       )
  //     )
  //   ),
  //   false
  // )
}

object JsonSample extends zio.App {
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _                      <- ZIO.unit
      person                 = Person("Michelle", 32)
      paymentMethod          = PaymentMethod.CreditCard("123456789", 1, 2020)
      customer               = Customer(person, paymentMethod)
      personToJsonTransducer = JsonCodec.encoder[Customer](Customer.schema)
      _                      <- ZIO.debug(s"Customer Schema: ${Customer.schema}")
      _                      <- ZIO.debug(s"Customer: $customer")
      _                      <- ZIO.debug(s"Customer dynamic value: ${Customer.schema.toDynamic(customer)}")
      _                      <- ZIO.debug(s"Schema AST for Customer: ${Customer.schema.ast}")
      _ <- ZStream(customer)
            .transduce(personToJsonTransducer)
            .transduce(ZTransducer.utf8Decode)
            .foreach(ZIO.debug)
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

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
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
