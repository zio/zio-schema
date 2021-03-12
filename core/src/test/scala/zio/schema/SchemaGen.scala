package zio.schema

import zio.Chunk
import zio.random.Random
import zio.test.{ Gen, Sized }

object SchemaGen {

  val anyPrimitive: Gen[Random, Schema.Primitive[_]] =
    StandardTypeGen.anyStandardType.map(Schema.Primitive(_))

  type PrimitiveAndGen[A] = (Schema.Primitive[A], Gen[Random with Sized, A])

  val anyPrimitiveAndGen: Gen[Random, PrimitiveAndGen[_]] =
    StandardTypeGen.anyStandardTypeAndGen.map {
      case (standardType, gen) => Schema.Primitive(standardType) -> gen
    }

  type PrimitiveAndValue[A] = (Schema.Primitive[A], A)

  val anyPrimitiveAndValue: Gen[Random with Sized, PrimitiveAndValue[_]] =
    for {
      (schema, gen) <- anyPrimitiveAndGen
      value         <- gen
    } yield schema -> value

  val anyOptional: Gen[Random with Sized, Schema.Optional[_]] =
    anySchema.map(Schema.Optional(_))

  type OptionalAndGen[A] = (Schema.Optional[A], Gen[Random with Sized, Option[A]])

  val anyOptionalAndGen: Gen[Random with Sized, OptionalAndGen[_]] =
    anyPrimitiveAndGen.map {
      case (schema, gen) => Schema.Optional(schema) -> Gen.option(gen)
    }

  type OptionalAndValue[A] = (Schema.Optional[A], Option[A])

  val anyOptionalAndValue: Gen[Random with Sized, OptionalAndValue[_]] =
    for {
      (schema, gen) <- anyOptionalAndGen
      value         <- gen
    } yield schema -> value

  val anyTuple: Gen[Random with Sized, Schema.Tuple[_, _]] =
    anySchema.zipWith(anySchema) { (a, b) =>
      Schema.Tuple(a, b)
    }

  type TupleAndGen[A, B] = (Schema.Tuple[A, B], Gen[Random with Sized, (A, B)])

  val anyTupleAndGen: Gen[Random with Sized, TupleAndGen[_, _]] =
    for {
      (schemaA, genA) <- anyPrimitiveAndGen
      (schemaB, genB) <- anyPrimitiveAndGen
    } yield Schema.Tuple(schemaA, schemaB) -> genA.zip(genB)

  type TupleAndValue[A, B] = (Schema.Tuple[A, B], (A, B))

  val anyTupleAndValue: Gen[Random with Sized, TupleAndValue[_, _]] =
    for {
      (schema, gen)    <- anyTupleAndGen
      (valueA, valueB) <- gen
    } yield schema -> ((valueA, valueB))

  val anySequence: Gen[Random with Sized, Schema.Sequence[_]] =
    anySchema.map(Schema.Sequence(_))

  type SequenceAndGen[A] = (Schema.Sequence[A], Gen[Random with Sized, Chunk[A]])

  val anySequenceAndGen: Gen[Random with Sized, SequenceAndGen[_]] =
    anyPrimitiveAndGen.map {
      case (schema, gen) =>
        Schema.Sequence(schema) -> Gen.chunkOf(gen)
    }

  type SequenceAndValue[A] = (Schema.Sequence[A], Chunk[A])

  val anySequenceAndValue: Gen[Random with Sized, SequenceAndValue[_]] =
    for {
      (schema, gen) <- anySequenceAndGen
      value         <- gen
    } yield schema -> value

  val anyEnumeration: Gen[Random with Sized, Schema.Enumeration] =
    Gen.mapOf(Gen.anyString, anySchema).map(Schema.Enumeration)

  type EnumerationAndGen = (Schema.Enumeration, Gen[Random with Sized, Map[String, _]])

  val anyEnumerationAndGen: Gen[Random with Sized, EnumerationAndGen] =
    for {
      keyToSchemaAndGen <- Gen.mapOf(Gen.anyString, anyPrimitiveAndGen)
    } yield {
      val structure = keyToSchemaAndGen.view.mapValues {
        case (schema, _) => schema
      }.toMap
      val keyValueGenerators = keyToSchemaAndGen.map {
        case (key, (_, gen)) => Gen.const(key).zip(gen)
      }.toSeq
      val gen = Gen.oneOf(keyValueGenerators: _*).map(Map(_))
      Schema.Enumeration(structure) -> gen
    }

  type EnumerationAndValue = (Schema.Enumeration, Map[String, _])

  val anyEnumerationAndValue: Gen[Random with Sized, EnumerationAndValue] =
    for {
      (schema, gen) <- anyEnumerationAndGen
      value         <- gen
    } yield schema -> value

  val anyRecord: Gen[Random with Sized, Schema.Record] =
    Gen.mapOf(Gen.anyString, anySchema).map(Schema.Record)

  type RecordAndGen = (Schema.Record, Gen[Random with Sized, Map[String, _]])

  val anyRecordAndGen: Gen[Random with Sized, RecordAndGen] =
    for {
      keyToSchemaAndGen <- Gen.mapOf(Gen.anyString, anyPrimitiveAndGen)
    } yield {
      val structure = keyToSchemaAndGen.view.mapValues {
        case (schema, _) => schema
      }.toMap
      val keyValueGenerators = keyToSchemaAndGen.map {
        case (key, (_, gen)) => Gen.const(key).zip(gen)
      }.toSeq
      val gen = keyValueGenerators.foldLeft[Gen[Random with Sized, Map[String, _]]](Gen.const(Map.empty)) {
        (acc, gen) =>
          for {
            map      <- acc
            keyValue <- gen
          } yield map + keyValue
      }
      Schema.Record(structure) -> gen
    }

  type RecordAndValue = (Schema.Record, Map[String, _])

  val anyRecordAndValue: Gen[Random with Sized, RecordAndValue] =
    for {
      (schema, gen) <- anyRecordAndGen
      value         <- gen
    } yield schema -> value

  val anyRecordOfRecordsAndValue: Gen[Random with Sized, RecordAndValue] =
    for {
      (schema1, gen1) <- anyRecordAndGen
      (schema2, gen2) <- anyRecordAndGen
      (schema3, gen3) <- anyRecordAndGen
      (key1, value1)  <- Gen.anyString.zip(gen1)
      (key2, value2)  <- Gen.anyString.zip(gen2)
      (key3, value3)  <- Gen.anyString.zip(gen3)
    } yield Schema.Record(Map(key1 -> schema1, key2 -> schema2, key3 -> schema3)) -> Map(
      (key1, value1),
      (key2, value2),
      (key3, value3)
    )

  type SequenceTransform[A] = Schema.Transform[Chunk[A], List[A]]

  val anySequenceTransform: Gen[Random with Sized, SequenceTransform[_]] = {
    anySequence.map(schema => transformSequence(schema))
  }

  type SequenceTransformAndGen[A] = (SequenceTransform[A], Gen[Random with Sized, List[A]])

  val anySequenceTransformAndGen: Gen[Random with Sized, SequenceTransformAndGen[_]] =
    anyPrimitiveAndGen.map {
      case (schema, gen) =>
        transformSequence(Schema.Sequence(schema)) -> Gen.listOf(gen)
    }

  // TODO: Add some random Left values.
  private def transformSequence[A](schema: Schema.Sequence[A]): SequenceTransform[A] =
    Schema.Transform[Chunk[A], List[A]](schema, chunk => Right(chunk.toList), list => Right(Chunk.fromIterable(list)))

  type SequenceTransformAndValue[A] = (SequenceTransform[A], List[A])

  val anySequenceTransformAndValue: Gen[Random with Sized, SequenceTransformAndValue[_]] =
    for {
      (schema, gen) <- anySequenceTransformAndGen
      value         <- gen
    } yield schema -> value

  type RecordTransform[A] = Schema.Transform[Map[String, _], A]

  val anyRecordTransform: Gen[Random with Sized, RecordTransform[_]] = {
    anyRecord.map(schema => transformRecord(schema))
  }

  type RecordTransformAndGen[A] = (RecordTransform[A], Gen[Random with Sized, A])

  // TODO: How do we generate a value of a type that we know nothing about?
  val anyRecordTransformAndGen: Gen[Random with Sized, RecordTransformAndGen[_]] =
    Gen.empty
//    anyRecordAndGen.map {
//      case (schema, gen) => transformRecord(schema) -> gen
//    }

  // TODO: Dynamically generate a case class.
  def transformRecord[A](schema: Schema.Record): RecordTransform[A] =
    Schema.Transform[Map[String, _], A](schema, _ => Left("Not implemented."), _ => Left("Not implemented."))

  type RecordTransformAndValue[A] = (RecordTransform[A], A)

  val anyRecordTransformAndValue: Gen[Random with Sized, RecordTransformAndValue[_]] =
    for {
      (schema, gen) <- anyRecordTransformAndGen
      value         <- gen
    } yield schema -> value

  type EnumerationTransform[A] = Schema.Transform[Map[String, _], A]

  val anyEnumerationTransform: Gen[Random with Sized, EnumerationTransform[_]] = {
    anyEnumeration.map(schema => transformEnumeration(schema))
  }

  type EnumerationTransformAndGen[A] = (EnumerationTransform[A], Gen[Random with Sized, A])

  // TODO: How do we generate a value of a type that we know nothing about?
  val anyEnumerationTransformAndGen: Gen[Random with Sized, EnumerationTransformAndGen[_]] =
    Gen.empty
//    anyEnumerationAndGen.map {
//      case (schema, gen) => transformEnumeration(schema) -> gen
//    }

  // TODO: Dynamically generate a sealed trait and case/value classes.
  def transformEnumeration[A](schema: Schema.Enumeration): EnumerationTransform[_] =
    Schema.Transform[Map[String, _], A](schema, _ => Left("Not implemented."), _ => Left("Not implemented."))

  type EnumerationTransformAndValue[A] = (EnumerationTransform[A], A)

  val anyEnumerationTransformAndValue: Gen[Random with Sized, EnumerationTransformAndValue[_]] =
    for {
      (schema, gen) <- anyEnumerationTransformAndGen
      value         <- gen
    } yield schema -> value

  val anyTransform: Gen[Random with Sized, Schema.Transform[_, _]] = Gen.oneOf(
    anySequenceTransform,
    anyRecordTransform,
    anyEnumerationTransform
  )

  type TransformAndValue[A] = (Schema.Transform[_, A], A)

  val anyTransformAndValue: Gen[Random with Sized, TransformAndValue[_]] =
    Gen.oneOf[Random with Sized, TransformAndValue[_]](
      anySequenceTransformAndValue,
      anyRecordTransformAndValue,
      anyEnumerationTransformAndValue
    )

  type TransformAndGen[A] = (Schema.Transform[_, A], Gen[Random with Sized, A])

  val anyTransformAndGen: Gen[Random with Sized, TransformAndGen[_]] =
    Gen.oneOf[Random with Sized, TransformAndGen[_]](
      anySequenceTransformAndGen,
      anyRecordTransformAndGen,
      anyEnumerationTransformAndGen
    )

  lazy val anySchema: Gen[Random with Sized, Schema[_]] = Gen.oneOf(
    anyPrimitive,
    anyOptional,
    anyTuple,
    anySequence,
    anyEnumeration,
    anyRecord,
    anyTransform
  )
}
