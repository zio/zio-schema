package zio.schema

import zio.prelude.Validation
import zio.test._
import zio.{ Chunk, NonEmptyChunk }

object DynamicValueValidationSpec extends ZIOSpecDefault {

  case class Person(name: String, age: Int)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  def spec: Spec[Any, Nothing] =
    suite("DynamicValue.validate")(
      test("succeeds for a matching primitive") {
        val dyn = DynamicValue.fromSchemaAndValue(Schema[Int], 42)
        assertTrue(dyn.validate[Int] == Validation.succeed(42))
      },
      test("fails for a mismatched primitive") {
        val dyn    = DynamicValue.fromSchemaAndValue(Schema[String], "hello")
        val result = dyn.validate[Int]
        assertTrue(result.isFailure)
      },
      test("succeeds for a valid record") {
        val person = Person("Alice", 30)
        val dyn    = DynamicValue.fromSchemaAndValue(Person.schema, person)
        assertTrue(dyn.validate[Person] == Validation.succeed(person))
      },
      test("accumulates multiple errors for an invalid Sequence") {
        val intDyn = DynamicValue.fromSchemaAndValue(Schema[Int], 1)
        val strDyn = DynamicValue.fromSchemaAndValue(Schema[String], "bad")
        val seqDyn = DynamicValue.Sequence(Chunk(intDyn, strDyn, strDyn))
        val result = seqDyn.validate[Chunk[Int]]
        val errors = result.fold(identity, _ => NonEmptyChunk("no errors"))
        assertTrue(errors.length >= 2)
      },
      test("succeeds for Some") {
        val dyn = DynamicValue.SomeValue(DynamicValue.fromSchemaAndValue(Schema[Int], 7))
        assertTrue(dyn.validate[Option[Int]] == Validation.succeed(Some(7)))
      },
      test("succeeds for None") {
        val dyn: DynamicValue = DynamicValue.NoneValue
        assertTrue(dyn.validate[Option[Int]] == Validation.succeed(None))
      },
      test("succeeds for a 2-tuple") {
        val dyn = DynamicValue.Tuple(
          DynamicValue.fromSchemaAndValue(Schema[Int], 1),
          DynamicValue.fromSchemaAndValue(Schema[String], "a")
        )
        assertTrue(dyn.validate[(Int, String)] == Validation.succeed((1, "a")))
      },
      test("succeeds for a List") {
        val dyn = DynamicValue.fromSchemaAndValue(Schema[List[Int]], List(1, 2, 3))
        assertTrue(dyn.validate[List[Int]] == Validation.succeed(List(1, 2, 3)))
      },
      test("returns failure for DynamicValue.Error") {
        val dyn = DynamicValue.Error("something went wrong")
        assertTrue(dyn.validate[Int].isFailure)
      }
    )

}
