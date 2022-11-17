package zio.schema

import scala.collection.immutable.ListMap

import zio.schema.Schema.Primitive
import zio.schema.SchemaGen._
import zio.test.Assertion._
import zio.test.{ Sized, TestConfig, _ }
import zio.{ Chunk, URIO }

object OrderingSpec extends ZIOSpecDefault {

  override def spec: Spec[Environment, Any] =
    suite("schemas should generate correct orderings")(
      suite("primitives")(primitiveOrderingTests: _*),
      suite("structures")(structureTestCases.map(structureOrderingTest): _*),
      suite("laws")(
        test("reflexivity") {
          check(anySchemaAndValue) {
            case (schema, a) => assert(schema.ordering.compare(a, a))(equalTo(0))
          }
        },
        test("antisymmetry") {
          check(genAnyOrderedPair) {
            case (schema, x, y) => assert(schema.ordering.compare(y, x))(isGreaterThan(0))
          }
        },
        test("transitivity") {
          check(genAnyOrderedTriplet) {
            case (schema, x, _, z) => assert(schema.ordering.compare(x, z))(isLessThan(0))
          }
        }
      )
    )

  val primitiveOrderingTests: List[Spec[Sized with TestConfig, Nothing]] =
    schemasAndGens.map {
      case SchemaTest(name, schema, gen) =>
        test(s"$name") {
          primitiveOrderingLaw(gen, schema)
        }
    }

  private def primitiveOrderingLaw[R, A](
    gen: Gen[R, A],
    standardType: StandardType[A]
  ): URIO[R with TestConfig, TestResult] =
    check(Gen.listOfN(2)(gen)) { lst =>
      val schema         = Primitive(standardType, Chunk.empty)
      val schemaOrdering = schema.ordering
      val l              = lst(0)
      val r              = lst(1)
      assert(schemaOrdering.compare(l, r))(equalTo(standardType.compare(l, r)))
    }

  case class StructureTestCase(name: String, increasingPairGen: Gen[Sized, SchemaAndPair[_]])

  private val structureTestCases =
    Seq(
      StructureTestCase("option", genAnyOrderedPairOption),
      StructureTestCase("either", genAnyOrderedPairEither),
      StructureTestCase("tuple", genAnyOrderedPairTuple),
      StructureTestCase("sequence", genAnyOrderedPairChunks),
      StructureTestCase("transform", genAnyOrderedPairTransform),
      StructureTestCase("record", genAnyOrderedPairRecord),
      StructureTestCase("enumN", genAnyOrderedPairEnum)
    )

  def structureOrderingTest(t: StructureTestCase): Spec[TestConfig with Sized, Nothing] =
    test(t.name)(check(t.increasingPairGen)(_ match {
      case (schema, l, r) =>
        assert(schema.ordering.compare(l, r))(isLessThan(0))
    }))

  def genAnyOrderedPairOption: Gen[Sized, SchemaAndPair[Option[_]]] =
    for {
      schema <- anySchema
      (l, r) <- genOrderedPairOption(schema)
    } yield (Schema.Optional(schema), l, r).asInstanceOf[SchemaAndPair[Option[_]]]

  def genOrderedPairOption[A](schema: Schema[A]): Gen[Sized, (Option[A], Option[A])] =
    Gen.oneOf(
      for {
        (smallA, largeA) <- genOrderedPair(schema)
      } yield (Some(smallA), Some(largeA)),
      for {
        a <- genFromSchema(schema)
      } yield (None, Some(a))
    )

  def genAnyOrderedPairEither: Gen[Sized, SchemaAndPair[Either[_, _]]] =
    for {
      leftSchema  <- anySchema
      rightSchema <- anySchema
      (l, r)      <- genOrderedPairEither(leftSchema, rightSchema)
    } yield (Schema.Either(leftSchema, rightSchema), l, r).asInstanceOf[SchemaAndPair[Either[_, _]]]

  def genOrderedPairEither[A, B](
    lSchema: Schema[A],
    rSchema: Schema[B]
  ): Gen[Sized, (Either[A, B], Either[A, B])] = Gen.oneOf(
    for {
      (small, large) <- genOrderedPair(lSchema)
    } yield (Left(small), Left(large)),
    for {
      (small, large) <- genOrderedPair(rSchema)
    } yield (Right(small), Right(large)),
    for {
      l <- genFromSchema(lSchema)
      r <- genFromSchema(rSchema)
    } yield (Left(l), Right(r))
  )

  def genAnyOrderedPairTuple: Gen[Sized, SchemaAndPair[_]] =
    for {
      xSchema <- anySchema
      ySchema <- anySchema
      (l, r)  <- genOrderedPairTuple(xSchema, ySchema)
    } yield (Schema.Tuple2(xSchema, ySchema), l, r).asInstanceOf[SchemaAndPair[Either[_, _]]]

  def genOrderedPairTuple[A, B](
    xSchema: Schema[A],
    ySchema: Schema[B]
  ): Gen[Sized, ((A, B), (A, B))] =
    Gen.oneOf(
      for {
        (smallX, largeX) <- genOrderedPair(xSchema)
        leftY            <- genFromSchema(ySchema)
        rightY           <- genFromSchema(ySchema)
      } yield ((smallX, leftY), (largeX, rightY)),
      for {
        x                <- genFromSchema(xSchema)
        (smallY, largeY) <- genOrderedPair(ySchema)
      } yield ((x, smallY), (x, largeY))
    )

  def genAnyOrderedPairChunks: Gen[Sized, SchemaAndPair[_]] =
    anySchema.flatMap(genOrderedPairChunk(_))

  def genOrderedPairChunk[A](schema: Schema[A]): Gen[Sized, SchemaAndPair[_]] =
    for {
      init <- Gen.chunkOf(genFromSchema(schema))
      rems <- Gen.oneOf(
               genChunkPairWithOrderedFirstElement(schema),
               genChunkPairWithOnlyFirstEmpty(schema)
             )
    } yield (Schema.chunk(schema), init ++ rems._1, init ++ rems._2)

  def genChunkPairWithOrderedFirstElement[A](
    schema: Schema[A]
  ): Gen[Sized, (Chunk[A], Chunk[A])] =
    for {
      inits <- genOrderedPair(schema)
      remL  <- Gen.chunkOf(genFromSchema(schema))
      remR  <- Gen.chunkOf(genFromSchema(schema))
    } yield (inits._1 +: remL, inits._2 +: remR)

  def genChunkPairWithOnlyFirstEmpty[A](schema: Schema[A]): Gen[Sized, (Chunk[A], Chunk[A])] =
    for {
      init <- genFromSchema(schema)
      rem  <- Gen.chunkOf(genFromSchema(schema))
    } yield (Chunk(), init +: rem)

  def genAnyOrderedPairTransform: Gen[Sized, SchemaAndPair[_]] =
    Gen.oneOf(
      anySchema.flatMap(genOrderedPairIdentityTransform(_)),
      anySchema.flatMap(genOrderedPairDecodeTransform(_))
    )

  def genOrderedPairIdentityTransform[A](schema: Schema[A]): Gen[Sized, SchemaAndPair[_]] =
    for {
      (small, large) <- genOrderedPair(schema)
    } yield (schema.transformOrFail({ (a: A) =>
      Right(a)
    }, { (a: A) =>
      Right(a)
    }), small, large)

  def genOrderedPairDecodeTransform[A](
    schema: Schema[A]
  ): Gen[Sized, SchemaAndPair[DynamicValue]] =
    for {
      error               <- Gen.boolean
      (small, large)      <- genOrderedPair(schema)
      encode              = (a: A) => Right(schema.toDynamic(a))
      decode              = schema.fromDynamic(_)
      smallEncoded        = encode(small).toOption.get
      smallEncodedOrError = if (error) DynamicValue.SomeValue(smallEncoded) else smallEncoded
      largeEncoded        = encode(large).toOption.get
    } yield (schema.transformOrFail(encode, decode), smallEncodedOrError, largeEncoded)

  def genAnyOrderedPairRecord: Gen[Sized, SchemaAndPair[_]] =
    for {
      name <- Gen.string(Gen.alphaChar).map(TypeId.parse)
      schema <- anyStructure(anyTree(1)).map(fieldSet => {
                 Schema.GenericRecord(name, fieldSet)
               })
      pair <- genOrderedPairRecord(schema)
    } yield pair

  def genOrderedPairRecord[A](schema: Schema.Record[A]): Gen[Sized, SchemaAndPair[A]] = {
    val fields: Chunk[Schema.Field[A, _]] = schema.fields
    for {
      diffInd       <- Gen.int(0, fields.size - 1)
      equalFields   <- genEqualFields(fields, 0, diffInd)
      orderedFields <- genOrderedField(fields(diffInd))
      randomFields  <- genRandomFields(fields, diffInd + 1)
      name          <- Gen.string(Gen.alphaChar).map(TypeId.parse)
    } yield {
      val allFields = (equalFields :+ orderedFields) ++ randomFields
      val xFields   = allFields.map { case (label, value, _) => (label, value) }
      val yFields   = allFields.map { case (label, _, value) => (label, value) }
      val x         = DynamicValue.Record(name, ListMap(xFields: _*)).toTypedValue(schema).toOption.get
      val y         = DynamicValue.Record(name, ListMap(yFields: _*)).toTypedValue(schema).toOption.get
      (schema, x, y)
    }
  }

  def genEqualFields[A](
    fields: Chunk[Schema.Field[A, _]],
    currentInd: Int,
    diffInd: Int
  ): Gen[Sized, Chunk[(String, DynamicValue, DynamicValue)]] =
    if (currentInd >= diffInd) Gen.const(Chunk())
    else {
      val field = fields(currentInd)
      for {
        x   <- genFromSchema(field.schema)
        d   = DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]], x)
        rem <- genEqualFields(fields, currentInd + 1, diffInd)
      } yield (field.name, d, d) +: rem
    }

  def genOrderedField[A](field: Schema.Field[A, _]): Gen[Sized, (String, DynamicValue, DynamicValue)] =
    genOrderedPair(field.schema).map {
      case (a, b) =>
        (
          field.name,
          DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]], a),
          DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]], b)
        )
    }

  def genRandomFields[A](
    fields: Chunk[Schema.Field[A, _]],
    currentInd: Int
  ): Gen[Sized, Chunk[(String, DynamicValue, DynamicValue)]] =
    if (currentInd >= fields.size) Gen.unit.map(_ => Chunk())
    else {
      val field = fields(currentInd)
      for {
        x   <- genFromSchema(field.schema)
        y   <- genFromSchema(field.schema)
        dx  = DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]], x)
        dy  = DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]], y)
        rem <- genRandomFields(fields, currentInd + 1)
      } yield (field.name, dx, dy) +: rem
    }

  def genAnyOrderedPairEnum: Gen[Sized, SchemaAndPair[_]] =
    anyEnumSchema.flatMap(schema => {
      Gen.oneOf(
        genOrderedPairEnumSameCase(schema),
        genOrderedPairEnumDiffCase(schema)
      )
    })

  def genOrderedPairEnumSameCase[A](schema: Schema.Enum[A]): Gen[Sized, SchemaAndPair[A]] =
    for {
      caseValue              <- Gen.elements(schema.cases.toList: _*)
      (smallCase, largeCase) <- genOrderedDynamicPair(caseValue.schema)
      id                     <- Gen.string(Gen.alphaChar).map(TypeId.parse)
      small                  = DynamicValue.Enumeration(id, (caseValue.id, smallCase)).toTypedValue(schema).toOption.get
      large                  = DynamicValue.Enumeration(id, (caseValue.id, largeCase)).toTypedValue(schema).toOption.get
    } yield (schema, small, large)

  def genOrderedPairEnumDiffCase[A](schema: Schema.Enum[A]): Gen[Sized, SchemaAndPair[A]] = {
    val cases = schema.cases
    for {
      smallInd  <- Gen.int(0, cases.size - 2)
      largeInd  <- Gen.int(smallInd + 1, cases.size - 1)
      smallCase = cases(smallInd)
      largeCase = cases(largeInd)
      small     <- genElemOfCase(schema, smallCase.id, smallCase.schema)
      large     <- genElemOfCase(schema, largeCase.id, largeCase.schema)
    } yield (schema, small, large)
  }

  def genElemOfCase[A, B](
    enumSchema: Schema.Enum[A],
    label: String,
    caseSchema: Schema[B]
  ): Gen[Sized, A] =
    genFromSchema(caseSchema).map(b => {
      val innerValue = caseSchema.toDynamic(b)
      val enumValue  = DynamicValue.Enumeration(enumSchema.id, (label, innerValue))
      enumValue.toTypedValue(enumSchema).toOption.get
    })

  type SchemaAndPair[A]    = (Schema[A], A, A)
  type SchemaAndTriplet[A] = (Schema[A], A, A, A)

  def genFromSchema[A](schema: Schema[A]): Gen[Sized, A] =
    DynamicValueGen
      .anyDynamicValueOfSchema(schema)
      .map(d => schema.fromDynamic(d).toOption.get)

  def genAnyOrderedPair: Gen[Sized, SchemaAndPair[_]] =
    for {
      schema <- anySchema
      (x, y) <- genOrderedPair(schema)
    } yield (schema, x, y).asInstanceOf[(Schema[Any], Any, Any)]

  def genOrderedPair[A](schema: Schema[A]): Gen[Sized, (A, A)] =
    (genFromSchema(schema) <*> genFromSchema(schema)).withFilter {
      case (l, r) => {
        schema.ordering.compare(l, r) < 0
      }
    }

  def genOrderedDynamicPair[A](schema: Schema[A]): Gen[Sized, (DynamicValue, DynamicValue)] =
    for {
      (small, large) <- genOrderedPair(schema)
    } yield (
      DynamicValue.fromSchemaAndValue(schema, small.asInstanceOf[A]),
      DynamicValue.fromSchemaAndValue(schema, large.asInstanceOf[A])
    )

  def genAnyOrderedTriplet: Gen[Sized, SchemaAndTriplet[_]] =
    for {
      schema    <- anySchema
      (x, y, z) <- genOrderedTriplet(schema)
    } yield (schema, x, y, z).asInstanceOf[(Schema[Any], Any, Any, Any)]

  def genOrderedTriplet[A](schema: Schema[A]): Gen[Sized, (A, A, A)] =
    for {
      (x, y) <- genOrderedPair(schema)
      z <- genFromSchema(schema).withFilter { cur =>
            schema.ordering.compare(y, cur) < 0
          }
    } yield (x, y, z)

}
