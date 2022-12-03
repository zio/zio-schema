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
    field01 =
      Schema.Field[Person, String]("name", Schema.primitive[String], get0 = _.name, set0 = (p, v) => p.copy(name = v)),
    field02 = Schema.Field[Person, Int]("age", Schema.primitive[Int], get0 = _.age, set0 = (p, v) => p.copy(age = v)),
    construct0 = (name, age) => Person(name, age)
  )

  val schemaPaymentMethodWireTransfer: Schema[WireTransfer] = Schema.CaseClass2[String, String, WireTransfer](
    TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.WireTransfer"),
    field01 = Schema.Field[WireTransfer, String](
      "accountNumber",
      Schema.primitive[String],
      get0 = _.accountNumber,
      set0 = (p, v) => p.copy(accountNumber = v)
    ),
    field02 = Schema.Field[WireTransfer, String](
      "bankCode",
      Schema.primitive[String],
      get0 = _.bankCode,
      set0 = (p, v) => p.copy(bankCode = v)
    ),
    construct0 = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode)
  )

  val schemaPaymentMethodCreditCard: Schema[CreditCard] = Schema.CaseClass3[String, Int, Int, CreditCard](
    TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.CreditCard"),
    field01 = Schema.Field[CreditCard, String](
      "number",
      Schema.primitive[String],
      get0 = _.number,
      set0 = (p, v) => p.copy(number = v)
    ),
    field02 = Schema.Field[CreditCard, Int](
      "expirationMonth",
      Schema.primitive[Int],
      get0 = _.expirationMonth,
      set0 = (p, v) => p.copy(expirationMonth = v)
    ),
    field03 = Schema.Field[CreditCard, Int](
      "expirationYear",
      Schema.primitive[Int],
      get0 = _.expirationYear,
      set0 = (p, v) => p.copy(expirationYear = v)
    ),
    construct0 =
      (number, expirationMonth, expirationYear) => PaymentMethod.CreditCard(number, expirationMonth, expirationYear)
  )

  val schemaPaymentMethod: Schema[PaymentMethod] =
    Schema.Enum2[PaymentMethod.CreditCard, PaymentMethod.WireTransfer, PaymentMethod](
      id = TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod"),
      case1 = Case[PaymentMethod, PaymentMethod.CreditCard](
        id = "CreditCard",
        schema = schemaPaymentMethodCreditCard,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.CreditCard],
        construct = pc => pc.asInstanceOf[PaymentMethod],
        isCase = _.isInstanceOf[PaymentMethod.CreditCard],
        annotations = Chunk.empty
      ),
      case2 = Case[PaymentMethod, PaymentMethod.WireTransfer](
        id = "WireTransfer",
        schema = schemaPaymentMethodWireTransfer,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.WireTransfer],
        construct = pc => pc.asInstanceOf[PaymentMethod],
        isCase = _.isInstanceOf[PaymentMethod.WireTransfer],
        annotations = Chunk.empty
      ),
      annotations = Chunk.empty
    )

  val schemaCustomer: Schema[Customer] = Schema.CaseClass2[Person, PaymentMethod, Customer](
    TypeId.parse("dev.zio.schema.example.example1.Domain.Customer"),
    field01 =
      Schema.Field[Customer, Person]("person", schemaPerson, get0 = _.person, set0 = (p, v) => p.copy(person = v)),
    field02 = Schema.Field[Customer, PaymentMethod](
      "paymentMethod",
      schemaPaymentMethod,
      get0 = _.paymentMethod,
      set0 = (p, v) => p.copy(paymentMethod = v)
    ),
    construct0 = (person, paymentMethod) => Customer(person, paymentMethod)
  )

  val schemaPersonDictionary: Schema[scala.collection.immutable.Map[String, Person]] =
    Schema.map(
      Schema.primitive[String],
      Schema[Person](schemaPerson)
    )

}

object MacroConstruction {

  implicit val schemaPerson: Schema[Person] = DeriveSchema.gen[Person]

  val schemaPaymentMethod: Schema[PaymentMethod] = DeriveSchema.gen[PaymentMethod]

  val schemaCustomer: Schema[Customer] = DeriveSchema.gen[Customer]

  val schemaPersonDictionaryFromMacro: Schema[scala.collection.immutable.Map[String, Person]] =
    DeriveSchema.gen[Map[String, Person]]

}

object JsonSample extends zio.ZIOAppDefault {
  import ManualConstruction._
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _                      <- ZIO.unit
      person                 = Person("Michelle", 32)
      personToJsonTransducer = JsonCodec.jsonBinaryCodec[Person](schemaPerson).streamEncoder
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

      personToProto = ProtobufCodec.protobufCodec[Person](schemaPerson).streamEncoder
      protoToPerson = ProtobufCodec.protobufCodec[Person](schemaPerson).streamDecoder

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

      personToJson = JsonCodec.jsonBinaryCodec[Person](schemaPerson).streamEncoder
      jsonToPerson = JsonCodec.jsonBinaryCodec[Person](schemaPerson).streamDecoder

      personToProto = ProtobufCodec.protobufCodec[Person](schemaPerson).streamEncoder
      protoToPerson = ProtobufCodec.protobufCodec[Person](schemaPerson).streamDecoder

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

object DictionaryExample extends ZIOAppDefault {

  import MacroConstruction._
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream
  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _          <- ZIO.unit
      person     = Person("Mike", 32)
      dictionary = Map("m" -> person)
      dictionaryToJson = JsonCodec
        .jsonBinaryCodec[scala.collection.immutable.Map[String, Person]](schemaPersonDictionaryFromMacro)
        .streamEncoder
      jsonToDictionary = JsonCodec
        .jsonBinaryCodec[scala.collection.immutable.Map[String, Person]](schemaPersonDictionaryFromMacro)
        .streamDecoder
      newPersonDictionary <- ZStream(dictionary)
                              .via(dictionaryToJson)
                              .via(jsonToDictionary)
                              .runHead
                              .some
                              .catchAll(error => ZIO.debug(error))
      _ <- ZIO.debug("is old dictionary the new dictionary? " + (dictionary == newPersonDictionary).toString)
      _ <- ZIO.debug("old dictionary: " + dictionary)
      _ <- ZIO.debug("new dictionary: " + newPersonDictionary)
    } yield ExitCode.success
}
