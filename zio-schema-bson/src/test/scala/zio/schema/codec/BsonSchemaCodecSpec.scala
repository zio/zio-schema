package zio.schema.codec

import scala.reflect.{ ClassTag, classTag }

import org.bson.codecs.configuration.CodecRegistry
import org.bson.codecs.{ Codec => BCodec, DecoderContext, EncoderContext }
import org.bson.conversions.Bson
import org.bson.io.BasicOutputBuffer
import org.bson.types.{ Decimal128, ObjectId }
import org.bson.{ BsonDecimal128, _ }

import zio.bson.BsonBuilder._
import zio.bson._
import zio.schema.{ DeriveGen, DeriveSchema, Schema }
import zio.test._
import zio.test.diff.Diff
import zio.{ Chunk, Scope, Task, UIO, ZIO }

object BsonSchemaCodecSpec extends ZIOSpecDefault {
  case class SimpleClass(a: String, b: Int)

  object SimpleClass {
    implicit val schema: Schema[SimpleClass]        = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[SimpleClass] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class BigDecimalClass(value: BigDecimal)

  object BigDecimalClass {
    implicit val schema: Schema[BigDecimalClass]        = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[BigDecimalClass] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait Tree

  object Tree {
    case class Branch(left: Tree, right: Tree) extends Tree
    case class Leaf(value: Int)                extends Tree

    implicit lazy val schema: Schema[Tree]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[Tree] = BsonSchemaCodec.bsonCodec(schema)

    private val genLeaf = Gen.int.map(Leaf)

    lazy val gen: Gen[Any, Tree] = Gen.sized { i =>
      if (i >= 2) Gen.oneOf(genLeaf, Gen.suspend(gen.zipWith(gen)(Branch)).resize(i / 2))
      else genLeaf
    }
  }

  sealed trait EnumLike

  object EnumLike {
    case object A extends EnumLike
    case object B extends EnumLike

    implicit lazy val schema: Schema[EnumLike]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[EnumLike] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class CustomerId(value: ObjectId) extends AnyVal
  case class Customer(id: CustomerId, name: String, age: Int, invitedFriends: List[CustomerId])

  object Customer {
    implicit lazy val customerIdSchema: Schema[CustomerId] = bson.ObjectIdSchema.transform(CustomerId(_), _.value)

    implicit lazy val customerSchema: Schema[Customer]   = DeriveSchema.gen[Customer]
    implicit lazy val customerCodec: BsonCodec[Customer] = BsonSchemaCodec.bsonCodec(customerSchema)

    val example: Customer = Customer(
      id = CustomerId(ObjectId.get),
      name = "Joseph",
      age = 18,
      invitedFriends = List(CustomerId(ObjectId.get), CustomerId(ObjectId.get))
    )

    lazy val genCustomerId: Gen[Any, CustomerId] =
      Gen.vectorOfN(12)(Gen.byte).map(bs => new ObjectId(bs.toArray)).map(CustomerId.apply)

    def gen: Gen[Sized, Customer] =
      for {
        id      <- genCustomerId
        name    <- Gen.string
        age     <- Gen.int
        friends <- Gen.listOf(genCustomerId)
      } yield Customer(id, name, age, friends)
  }

  // Custom generator for BigDecimal values with rounding to ensure exact representation as Decimal128
  def genRoundedBigDecimal(scale: Int): Gen[Any, BigDecimal] =
    Gen.double.map(d => BigDecimal(d).setScale(scale, BigDecimal.RoundingMode.HALF_UP))

  def spec: Spec[TestEnvironment with Scope, Any] = suite("BsonSchemaCodecSpec")(
    suite("round trip")(
      roundTripTest("SimpleClass")(
        DeriveGen.gen[SimpleClass],
        SimpleClass("str", 1),
        doc("a" -> str("str"), "b" -> int(1))
      ),
      roundTripTest("Tree")(
        Tree.gen,
        Tree.Leaf(1),
        doc("Leaf" -> doc("value" -> int(1)))
      ),
      roundTripTest[EnumLike]("EnumLike")(
        Gen.fromIterable(Chunk(EnumLike.A, EnumLike.B)),
        EnumLike.A,
        str("A")
      ),
      roundTripTest("Customer")(
        Customer.gen,
        Customer.example,
        doc(
          "id"   -> Customer.example.id.value.toBsonValue,
          "name" -> str(Customer.example.name),
          "age"  -> int(Customer.example.age),
          "invitedFriends" -> array(
            Customer.example.invitedFriends.map(_.value.toBsonValue): _*
          )
        )
      ),
      roundTripTest("BigDecimalClass")(
        // 14 decimal places in the assert value below
        genRoundedBigDecimal(14).map(BigDecimalClass(_)),
        BigDecimalClass(BigDecimal("279.00000000000000")),
        doc("value" -> new BsonDecimal128(Decimal128.parse("279.00000000000000")))
      )
    ),
    suite("configuration")(
      suite("schema annotations")(
        roundTripExamples[SchemaConfig.NoDiscriminator]("no discriminator")(
          SchemaConfig.NoDiscriminator.A("str") -> doc("a" -> str("str")),
          SchemaConfig.NoDiscriminator.B("str") -> doc("b" -> str("str")),
          SchemaConfig.NoDiscriminator.C("str") -> doc("c" -> str("str"))
        ),
        roundTripExamples[SchemaConfig.WithoutDiscriminator]("without discriminator")(
          SchemaConfig.WithoutDiscriminator.A("str") -> doc("A" -> doc("s" -> str("str"))),
          SchemaConfig.WithoutDiscriminator.B("str") -> doc("B" -> doc("s" -> str("str")))
        ),
        roundTripExamples[SchemaConfig.WithDiscriminator]("with discriminator")(
          SchemaConfig.WithDiscriminator.A("str") -> doc("$type" -> str("A"), "s" -> str("str")),
          SchemaConfig.WithDiscriminator.B("str") -> doc("$type" -> str("B"), "s" -> str("str"))
        ),
        roundTripExamples[SchemaConfig.CaseNameEnumLike]("caseName enum like")(
          SchemaConfig.CaseNameEnumLike.A -> str("aName"),
          SchemaConfig.CaseNameEnumLike.B -> str("bName")
        ),
        roundTripExamples[SchemaConfig.CaseNameWithoutDiscriminator]("caseName without discriminator")(
          SchemaConfig.CaseNameWithoutDiscriminator.A("str") -> doc("aName" -> doc("s" -> str("str"))),
          SchemaConfig.CaseNameWithoutDiscriminator.B("str") -> doc("bName" -> doc("s" -> str("str")))
        ),
        roundTripExamples[SchemaConfig.CaseNameWithDiscriminator]("caseName with discriminator")(
          SchemaConfig.CaseNameWithDiscriminator.A("str") -> doc("$type" -> str("aName"), "s" -> str("str")),
          SchemaConfig.CaseNameWithDiscriminator.B("str") -> doc("$type" -> str("bName"), "s" -> str("str"))
        ),
        suite("caseNameAliases without discriminator")(
          testEncodeExamples[SchemaConfig.CaseNameAliasesWithoutDiscriminator](
            SchemaConfig.CaseNameAliasesWithoutDiscriminator.A("str") -> doc("A" -> doc("s" -> str("str"))),
            SchemaConfig.CaseNameAliasesWithoutDiscriminator.B("str") -> doc("B" -> doc("s" -> str("str")))
          ),
          testDecodeExamples[SchemaConfig.CaseNameAliasesWithoutDiscriminator](
            doc("A"       -> doc("s" -> str("str"))) -> SchemaConfig.CaseNameAliasesWithoutDiscriminator.A("str"),
            doc("B"       -> doc("s" -> str("str"))) -> SchemaConfig.CaseNameAliasesWithoutDiscriminator.B("str"),
            doc("aAlias1" -> doc("s" -> str("str"))) -> SchemaConfig.CaseNameAliasesWithoutDiscriminator.A("str"),
            doc("bAlias1" -> doc("s" -> str("str"))) -> SchemaConfig.CaseNameAliasesWithoutDiscriminator.B("str"),
            doc("aAlias2" -> doc("s" -> str("str"))) -> SchemaConfig.CaseNameAliasesWithoutDiscriminator.A("str"),
            doc("bAlias2" -> doc("s" -> str("str"))) -> SchemaConfig.CaseNameAliasesWithoutDiscriminator.B("str")
          )
        ),
        suite("caseNameAliases with discriminator")(
          testEncodeExamples[SchemaConfig.CaseNameAliasesWithDiscriminator](
            SchemaConfig.CaseNameAliasesWithDiscriminator.A("str") -> doc("$type" -> str("A"), "s" -> str("str")),
            SchemaConfig.CaseNameAliasesWithDiscriminator.B("str") -> doc("$type" -> str("B"), "s" -> str("str"))
          ),
          testDecodeExamples[SchemaConfig.CaseNameAliasesWithDiscriminator](
            doc("$type" -> str("A"), "s"       -> str("str")) -> SchemaConfig.CaseNameAliasesWithDiscriminator.A("str"),
            doc("$type" -> str("B"), "s"       -> str("str")) -> SchemaConfig.CaseNameAliasesWithDiscriminator.B("str"),
            doc("$type" -> str("aAlias1"), "s" -> str("str")) -> SchemaConfig.CaseNameAliasesWithDiscriminator.A("str"),
            doc("$type" -> str("bAlias1"), "s" -> str("str")) -> SchemaConfig.CaseNameAliasesWithDiscriminator.B("str"),
            doc("$type" -> str("aAlias2"), "s" -> str("str")) -> SchemaConfig.CaseNameAliasesWithDiscriminator.A("str"),
            doc("$type" -> str("bAlias2"), "s" -> str("str")) -> SchemaConfig.CaseNameAliasesWithDiscriminator.B("str")
          )
        ),
        roundTripExamples[SchemaConfig.FieldName]("fieldName")(
          SchemaConfig.FieldName(a = "str") -> doc("customName" -> str("str"))
        ),
        suite("fieldDefaultValue")(
          testEncodeExamples[SchemaConfig.FieldDefaultValue](
            SchemaConfig.FieldDefaultValue("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamples[SchemaConfig.FieldDefaultValue](
            doc() -> SchemaConfig.FieldDefaultValue("defaultValue")
          )
        ),
        suite("allowExtraFields")(
          testEncodeExamples[SchemaConfig.AllowExtraFields](
            SchemaConfig.AllowExtraFields("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamples[SchemaConfig.AllowExtraFields](
            doc("extra" -> doc(), "a" -> str("str")) -> SchemaConfig.AllowExtraFields("str")
          )
        ),
        suite("rejectExtraFields")(
          testEncodeExamples[SchemaConfig.RejectExtraFields](
            SchemaConfig.RejectExtraFields("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamplesError[SchemaConfig.RejectExtraFields](
            doc("extra" -> doc(), "a" -> str("str"))
          )(".extra")
        ),
        suite("transientCase without discriminator")(
          testEncodeExamples[SchemaConfig.TransientCaseWithoutDiscriminator](
            SchemaConfig.TransientCaseWithoutDiscriminator.A("str") -> doc("A" -> doc("s" -> str("str"))),
            SchemaConfig.TransientCaseWithoutDiscriminator.B("str") -> doc("B" -> doc("s" -> str("str"))),
            SchemaConfig.TransientCaseWithoutDiscriminator.C("str") -> doc()
          ),
          testDecodeExamples[SchemaConfig.TransientCaseWithoutDiscriminator](
            doc("A" -> doc("s" -> str("str"))) -> SchemaConfig.TransientCaseWithoutDiscriminator.A("str"),
            doc("B" -> doc("s" -> str("str"))) -> SchemaConfig.TransientCaseWithoutDiscriminator.B("str")
          )
        ),
        suite("transientCase with discriminator")(
          testEncodeExamples[SchemaConfig.TransientCaseWithDiscriminator](
            SchemaConfig.TransientCaseWithDiscriminator.A("str") -> doc("$type" -> str("A"), "s" -> str("str")),
            SchemaConfig.TransientCaseWithDiscriminator.B("str") -> doc("$type" -> str("B"), "s" -> str("str")),
            SchemaConfig.TransientCaseWithDiscriminator.C("str") -> doc()
          ),
          testDecodeExamples[SchemaConfig.TransientCaseWithDiscriminator](
            doc("$type" -> str("A"), "s" -> str("str")) -> SchemaConfig.TransientCaseWithDiscriminator.A("str"),
            doc("$type" -> str("B"), "s" -> str("str")) -> SchemaConfig.TransientCaseWithDiscriminator.B("str")
          )
        ),
        suite("transientField")(
          testEncodeExamples[SchemaConfig.TransientField](
            SchemaConfig.TransientField("str", 1) -> doc("b" -> int(1))
          ),
          testDecodeExamples[SchemaConfig.TransientField](
            doc("b" -> int(1)) -> SchemaConfig.TransientField("defaultValue", 1),
            doc("a" -> str("str"), "b" -> int(1)) -> SchemaConfig.TransientField("str", 1)
          )
        )
      ),
      suite("bson annotations")(
        roundTripExamples[BsonConfig.WithoutDiscriminator]("without discriminator")(
          BsonConfig.WithoutDiscriminator.A("str") -> doc("A" -> doc("s" -> str("str"))),
          BsonConfig.WithoutDiscriminator.B("str") -> doc("B" -> doc("s" -> str("str")))
        ),
        roundTripExamples[BsonConfig.WithDiscriminator]("with discriminator")(
          BsonConfig.WithDiscriminator.A("str") -> doc("$type" -> str("A"), "s" -> str("str")),
          BsonConfig.WithDiscriminator.B("str") -> doc("$type" -> str("B"), "s" -> str("str"))
        ),
        roundTripExamples[BsonConfig.CaseNameEnumLike]("caseName enum like")(
          BsonConfig.CaseNameEnumLike.A -> str("aName"),
          BsonConfig.CaseNameEnumLike.B -> str("bName")
        ),
        roundTripExamples[BsonConfig.CaseNameWithoutDiscriminator]("caseName without discriminator")(
          BsonConfig.CaseNameWithoutDiscriminator.A("str") -> doc("aName" -> doc("s" -> str("str"))),
          BsonConfig.CaseNameWithoutDiscriminator.B("str") -> doc("bName" -> doc("s" -> str("str")))
        ),
        roundTripExamples[BsonConfig.CaseNameWithDiscriminator]("caseName with discriminator")(
          BsonConfig.CaseNameWithDiscriminator.A("str") -> doc("$type" -> str("aName"), "s" -> str("str")),
          BsonConfig.CaseNameWithDiscriminator.B("str") -> doc("$type" -> str("bName"), "s" -> str("str"))
        ),
        roundTripExamples[BsonConfig.FieldName]("fieldName")(
          BsonConfig.FieldName(a = "str") -> doc("customName" -> str("str"))
        ),
        suite("allowExtraFields")(
          testEncodeExamples[BsonConfig.AllowExtraFields](
            BsonConfig.AllowExtraFields("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamples[BsonConfig.AllowExtraFields](
            doc("extra" -> doc(), "a" -> str("str")) -> BsonConfig.AllowExtraFields("str")
          )
        ),
        suite("rejectExtraFields")(
          testEncodeExamples[BsonConfig.RejectExtraFields](
            BsonConfig.RejectExtraFields("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamplesError[BsonConfig.RejectExtraFields](
            doc("extra" -> doc(), "a" -> str("str"))
          )(".extra")
        ),
        suite("transientField")(
          testEncodeExamples[BsonConfig.TransientField](
            BsonConfig.TransientField("str", 1) -> doc("b" -> int(1))
          ),
          testDecodeExamples[BsonConfig.TransientField](
            doc("b" -> int(1)) -> BsonConfig.TransientField("defaultValue", 1),
            doc("a" -> str("str"), "b" -> int(1)) -> BsonConfig.TransientField("str", 1)
          )
        )
      ),
      suite("mixed annotations (bson priority)")(
        roundTripExamples[MixedConfig.NoDiscriminator]("no discriminator")(
          MixedConfig.NoDiscriminator.A("str") -> doc("a" -> str("str")),
          MixedConfig.NoDiscriminator.B("str") -> doc("b" -> str("str")),
          MixedConfig.NoDiscriminator.C("str") -> doc("c" -> str("str"))
        ),
        roundTripExamples[MixedConfig.WithoutDiscriminator]("without discriminator")(
          MixedConfig.WithoutDiscriminator.A("str") -> doc("A" -> doc("s" -> str("str"))),
          MixedConfig.WithoutDiscriminator.B("str") -> doc("B" -> doc("s" -> str("str")))
        ),
        roundTripExamples[MixedConfig.WithDiscriminator]("with discriminator")(
          MixedConfig.WithDiscriminator.A("str") -> doc("$type" -> str("A"), "s" -> str("str")),
          MixedConfig.WithDiscriminator.B("str") -> doc("$type" -> str("B"), "s" -> str("str"))
        ),
        roundTripExamples[MixedConfig.CaseNameEnumLike]("caseName enum like")(
          MixedConfig.CaseNameEnumLike.A -> str("aName"),
          MixedConfig.CaseNameEnumLike.B -> str("bName")
        ),
        roundTripExamples[MixedConfig.CaseNameWithoutDiscriminator]("caseName without discriminator")(
          MixedConfig.CaseNameWithoutDiscriminator.A("str") -> doc("aName" -> doc("s" -> str("str"))),
          MixedConfig.CaseNameWithoutDiscriminator.B("str") -> doc("bName" -> doc("s" -> str("str")))
        ),
        roundTripExamples[MixedConfig.CaseNameWithDiscriminator]("caseName with discriminator")(
          MixedConfig.CaseNameWithDiscriminator.A("str") -> doc("$type" -> str("aName"), "s" -> str("str")),
          MixedConfig.CaseNameWithDiscriminator.B("str") -> doc("$type" -> str("bName"), "s" -> str("str"))
        ),
        suite("caseNameAliases without discriminator")(
          testEncodeExamples[MixedConfig.CaseNameAliasesWithoutDiscriminator](
            MixedConfig.CaseNameAliasesWithoutDiscriminator.A("str") -> doc("aName" -> doc("s" -> str("str"))),
            MixedConfig.CaseNameAliasesWithoutDiscriminator.B("str") -> doc("B"     -> doc("s" -> str("str")))
          ),
          testDecodeExamples[MixedConfig.CaseNameAliasesWithoutDiscriminator](
            doc("aName"   -> doc("s" -> str("str"))) -> MixedConfig.CaseNameAliasesWithoutDiscriminator.A("str"),
            doc("B"       -> doc("s" -> str("str"))) -> MixedConfig.CaseNameAliasesWithoutDiscriminator.B("str"),
            doc("aAlias1" -> doc("s" -> str("str"))) -> MixedConfig.CaseNameAliasesWithoutDiscriminator.A("str"),
            doc("bAlias1" -> doc("s" -> str("str"))) -> MixedConfig.CaseNameAliasesWithoutDiscriminator.B("str"),
            doc("aAlias2" -> doc("s" -> str("str"))) -> MixedConfig.CaseNameAliasesWithoutDiscriminator.A("str"),
            doc("bAlias2" -> doc("s" -> str("str"))) -> MixedConfig.CaseNameAliasesWithoutDiscriminator.B("str")
          )
        ),
        suite("caseNameAliases with discriminator")(
          testEncodeExamples[MixedConfig.CaseNameAliasesWithDiscriminator](
            MixedConfig.CaseNameAliasesWithDiscriminator.A("str") -> doc("$type" -> str("aName"), "s" -> str("str")),
            MixedConfig.CaseNameAliasesWithDiscriminator.B("str") -> doc("$type" -> str("B"), "s"     -> str("str"))
          ),
          testDecodeExamples[MixedConfig.CaseNameAliasesWithDiscriminator](
            doc("$type" -> str("aName"), "s"   -> str("str")) -> MixedConfig.CaseNameAliasesWithDiscriminator.A("str"),
            doc("$type" -> str("B"), "s"       -> str("str")) -> MixedConfig.CaseNameAliasesWithDiscriminator.B("str"),
            doc("$type" -> str("aAlias1"), "s" -> str("str")) -> MixedConfig.CaseNameAliasesWithDiscriminator.A("str"),
            doc("$type" -> str("bAlias1"), "s" -> str("str")) -> MixedConfig.CaseNameAliasesWithDiscriminator.B("str"),
            doc("$type" -> str("aAlias2"), "s" -> str("str")) -> MixedConfig.CaseNameAliasesWithDiscriminator.A("str"),
            doc("$type" -> str("bAlias2"), "s" -> str("str")) -> MixedConfig.CaseNameAliasesWithDiscriminator.B("str")
          )
        ),
        roundTripExamples[MixedConfig.FieldName]("fieldName")(
          MixedConfig.FieldName(a = "str") -> doc("customName" -> str("str"))
        ),
        suite("fieldDefaultValue")(
          testEncodeExamples[MixedConfig.FieldDefaultValue](
            MixedConfig.FieldDefaultValue("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamples[MixedConfig.FieldDefaultValue](
            doc() -> MixedConfig.FieldDefaultValue("defaultValue")
          )
        ),
        suite("allowExtraFields")(
          testEncodeExamples[MixedConfig.AllowExtraFields](
            MixedConfig.AllowExtraFields("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamples[MixedConfig.AllowExtraFields](
            doc("extra" -> doc(), "a" -> str("str")) -> MixedConfig.AllowExtraFields("str")
          )
        ),
        suite("rejectExtraFields")(
          testEncodeExamples[MixedConfig.RejectExtraFields](
            MixedConfig.RejectExtraFields("str") -> doc("a" -> str("str"))
          ),
          testDecodeExamplesError[MixedConfig.RejectExtraFields](
            doc("extra" -> doc(), "a" -> str("str"))
          )(".extra")
        ),
        suite("transientCase without discriminator")(
          testEncodeExamples[MixedConfig.TransientCaseWithoutDiscriminator](
            MixedConfig.TransientCaseWithoutDiscriminator.A("str") -> doc("A" -> doc("s" -> str("str"))),
            MixedConfig.TransientCaseWithoutDiscriminator.B("str") -> doc("B" -> doc("s" -> str("str"))),
            MixedConfig.TransientCaseWithoutDiscriminator.C("str") -> doc()
          ),
          testDecodeExamples[MixedConfig.TransientCaseWithoutDiscriminator](
            doc("A" -> doc("s" -> str("str"))) -> MixedConfig.TransientCaseWithoutDiscriminator.A("str"),
            doc("B" -> doc("s" -> str("str"))) -> MixedConfig.TransientCaseWithoutDiscriminator.B("str")
          )
        ),
        suite("transientCase with discriminator")(
          testEncodeExamples[MixedConfig.TransientCaseWithDiscriminator](
            MixedConfig.TransientCaseWithDiscriminator.A("str") -> doc("$type" -> str("A"), "s" -> str("str")),
            MixedConfig.TransientCaseWithDiscriminator.B("str") -> doc("$type" -> str("B"), "s" -> str("str")),
            MixedConfig.TransientCaseWithDiscriminator.C("str") -> doc()
          ),
          testDecodeExamples[MixedConfig.TransientCaseWithDiscriminator](
            doc("$type" -> str("A"), "s" -> str("str")) -> MixedConfig.TransientCaseWithDiscriminator.A("str"),
            doc("$type" -> str("B"), "s" -> str("str")) -> MixedConfig.TransientCaseWithDiscriminator.B("str")
          )
        ),
        suite("transientField")(
          testEncodeExamples[MixedConfig.TransientField](
            MixedConfig.TransientField("str", 1) -> doc("b" -> int(1))
          ),
          testDecodeExamples[MixedConfig.TransientField](
            doc("b" -> int(1)) -> MixedConfig.TransientField("defaultValue", 1),
            doc("a" -> str("str"), "b" -> int(1)) -> MixedConfig.TransientField("str", 1)
          )
        )
      )
    )
  )

  val emptyCodecRegistry: CodecRegistry = new CodecRegistry {
    def get[T](clazz: Class[T]): BCodec[T] = null

    def get[T](clazz: Class[T], registry: CodecRegistry): BCodec[T] = null
  }

  def writeValue[T](value: T, codec: BCodec[T], writer: BsonWriter, isDocument: Boolean): UIO[Unit] =
    ZIO.succeed {
      if (isDocument) codec.encode(writer, value, EncoderContext.builder().build())
      else {
        writer.writeStartDocument()
        writer.writeName("v")
        codec.encode(writer, value, EncoderContext.builder().build())
        writer.writeEndDocument()
      }
    }

  def tryReadValue[T](codec: BCodec[T], reader: BsonReader, isDocument: Boolean): Task[T] =
    ZIO.attempt {
      if (isDocument) codec.decode(reader, DecoderContext.builder().build())
      else {
        reader.readStartDocument()
        reader.readBsonType()
        reader.skipName()
        val res = codec.decode(reader, DecoderContext.builder().build())
        reader.readEndDocument()
        res
      }
    }

  def readValue[T](codec: BCodec[T], reader: BsonReader, isDocument: Boolean): UIO[T] =
    tryReadValue[T](codec, reader, isDocument).orDie

  private def testEncodeExamples[T](
    examples: (T, BsonValue)*
  )(implicit encoder: BsonEncoder[T], decoder: BsonDecoder[T], ct: ClassTag[T], diff: Diff[T] = Diff.anyDiff[T]) = {
    val _ = diff

    suite("encode")(
      test("toBsonValue") {
        checkAll(Gen.fromIterable(examples)) {
          case (value, expectedBson) =>
            assertTrue(value.toBsonValue == expectedBson)
        }
      },
      test("write") {
        checkAll(Gen.fromIterable(examples)) {
          case (value, expectedBson) =>
            for {
              buffer        <- ZIO.fromAutoCloseable(ZIO.succeed(new BasicOutputBuffer()))
              writer        <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryWriter(buffer)))
              documentCodec = Bson.DEFAULT_CODEC_REGISTRY.get(classOf[BsonDocument])
              codec <- ZIO
                        .succeed(
                          zioBsonCodecProvider[T]
                            .get[T](classTag[T].runtimeClass.asInstanceOf[Class[T]], emptyCodecRegistry)
                        )
              _         <- writeValue(value, codec, writer, false)
              reader    <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryReader(buffer.getByteBuffers.get(0).asNIO())))
              document  <- readValue(documentCodec, reader, isDocument = true)
              bsonValue = document.get("v")
            } yield assertTrue(bsonValue == expectedBson)
        }
      }
    )

  }

  private def testDecodeExamplesError[T](
    examples: BsonValue*
  )(
    substring: String
  )(implicit encoder: BsonEncoder[T], decoder: BsonDecoder[T], ct: ClassTag[T], diff: Diff[T] = Diff.anyDiff[T]) = {
    val _ = diff

    suite("decode")(
      test("as") {
        checkAll(Gen.fromIterable(examples)) { bson =>
          assertTrue(bson.as[T].swap.toOption.get.contains(substring))
        }
      },
      test("read") {
        checkAll(Gen.fromIterable(examples)) {
          bsonValue =>
            for {
              buffer     <- ZIO.fromAutoCloseable(ZIO.succeed(new BasicOutputBuffer()))
              writer     <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryWriter(buffer)))
              valueCodec = Bson.DEFAULT_CODEC_REGISTRY.get(bsonValue.getClass).asInstanceOf[BCodec[BsonValue]]
              codec <- ZIO
                        .succeed(
                          zioBsonCodecProvider[T]
                            .get[T](classTag[T].runtimeClass.asInstanceOf[Class[T]], emptyCodecRegistry)
                        )
              _      <- writeValue(bsonValue, valueCodec, writer, false)
              reader <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryReader(buffer.getByteBuffers.get(0).asNIO())))
              res    <- tryReadValue(codec, reader, false).either
            } yield assertTrue(res.swap.toOption.get.getMessage.contains(substring))
        }
      }
    )
  }

  private def testDecodeExamples[T](
    examples: (BsonValue, T)*
  )(implicit encoder: BsonEncoder[T], decoder: BsonDecoder[T], ct: ClassTag[T], diff: Diff[T] = Diff.anyDiff[T]) = {
    val _ = diff

    suite("decode")(
      test("as") {
        checkAll(Gen.fromIterable(examples)) {
          case (bson, expected) =>
            assertTrue(bson.as[T].toOption.get == expected)
        }
      },
      test("read") {
        checkAll(Gen.fromIterable(examples)) {
          case (bsonValue, expected) =>
            for {
              buffer     <- ZIO.fromAutoCloseable(ZIO.succeed(new BasicOutputBuffer()))
              writer     <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryWriter(buffer)))
              valueCodec = Bson.DEFAULT_CODEC_REGISTRY.get(bsonValue.getClass).asInstanceOf[BCodec[BsonValue]]
              codec <- ZIO
                        .succeed(
                          zioBsonCodecProvider[T]
                            .get[T](classTag[T].runtimeClass.asInstanceOf[Class[T]], emptyCodecRegistry)
                        )
              _      <- writeValue(bsonValue, valueCodec, writer, false)
              reader <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryReader(buffer.getByteBuffers.get(0).asNIO())))
              res    <- readValue(codec, reader, false)
            } yield assertTrue(res == expected)
        }
      }
    )
  }

  private def roundTripExamples[T](name: String)(
    examples: (T, BsonValue)*
  )(implicit encoder: BsonEncoder[T], decoder: BsonDecoder[T], ct: ClassTag[T], diff: Diff[T] = Diff.anyDiff[T]) = {
    val _ = diff

    suite(name)(
      testEncodeExamples(examples: _*),
      testDecodeExamples(examples.map(_.swap): _*)
    )
  }

  private def roundTripTests[T](
    gen: Gen[Sized, T],
    example: T,
    bsonExample: BsonValue,
    isDocument: Boolean
  )(implicit encoder: BsonEncoder[T], decoder: BsonDecoder[T], ct: ClassTag[T], diff: Diff[T]) = {
    val _ = diff
    test("example") {
      assertTrue(example.toBsonValue == bsonExample)
    } +
      test("toBsonValue/as") {
        check(gen) { t =>
          assertTrue(t.toBsonValue.as[T].toOption.get == t)
        }
      } +
      test("writer/reader") {
        check(gen) { t =>
          for {
            buffer <- ZIO.fromAutoCloseable(ZIO.succeed(new BasicOutputBuffer()))
            writer <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryWriter(buffer)))
            codec <- ZIO
                      .succeed(
                        zioBsonCodecProvider[T]
                          .get[T](classTag[T].runtimeClass.asInstanceOf[Class[T]], emptyCodecRegistry)
                      )
            _      <- writeValue(t, codec, writer, isDocument)
            reader <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryReader(buffer.getByteBuffers.get(0).asNIO())))
            res    <- readValue(codec, reader, isDocument)
          } yield assertTrue(res == t)
        }
      } +
      test("toBsonValue/reader") {
        check(gen) { t =>
          for {
            buffer     <- ZIO.fromAutoCloseable(ZIO.succeed(new BasicOutputBuffer()))
            writer     <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryWriter(buffer)))
            bsonValue  = t.toBsonValue
            valueCodec = Bson.DEFAULT_CODEC_REGISTRY.get(bsonValue.getClass).asInstanceOf[BCodec[BsonValue]]
            codec <- ZIO
                      .succeed(
                        zioBsonCodecProvider[T]
                          .get[T](classTag[T].runtimeClass.asInstanceOf[Class[T]], emptyCodecRegistry)
                      )
            _      <- writeValue(bsonValue, valueCodec, writer, isDocument)
            reader <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryReader(buffer.getByteBuffers.get(0).asNIO())))
            res    <- readValue(codec, reader, isDocument)
          } yield assertTrue(res == t)
        }
      } +
      test("writer/as") {
        check(gen) { t =>
          for {
            buffer        <- ZIO.fromAutoCloseable(ZIO.succeed(new BasicOutputBuffer()))
            writer        <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryWriter(buffer)))
            documentCodec = Bson.DEFAULT_CODEC_REGISTRY.get(classOf[BsonDocument])
            codec <- ZIO
                      .succeed(
                        zioBsonCodecProvider[T]
                          .get[T](classTag[T].runtimeClass.asInstanceOf[Class[T]], emptyCodecRegistry)
                      )
            _        <- writeValue(t, codec, writer, isDocument)
            reader   <- ZIO.fromAutoCloseable(ZIO.succeed(new BsonBinaryReader(buffer.getByteBuffers.get(0).asNIO())))
            document <- readValue(documentCodec, reader, isDocument = true)
            bsonValue = if (isDocument) document
            else document.get("v")
            res = bsonValue.as[T]
          } yield assertTrue(res.toOption.get == t)
        }
      }
  }

  def roundTripTest[T: ClassTag](name: String)(
    gen: Gen[Sized, T],
    example: T,
    bsonExample: BsonValue
  )(
    implicit encoder: BsonEncoder[T],
    decoder: BsonDecoder[T],
    codec: BsonCodec[T],
    diff: Diff[T] = Diff.anyDiff[T]
  ): Spec[Sized with Any with Scope, Nothing] = {
    val isDocument = bsonExample.isDocument
    suite(name)(
      suite("encoder/decoder") {
        roundTripTests(gen, example, bsonExample, isDocument)(encoder, decoder, implicitly, implicitly)
      },
      suite("codec") {
        roundTripTests(gen, example, bsonExample, isDocument)(codec.encoder, codec.decoder, implicitly, implicitly)
      }
    )
  }
}
