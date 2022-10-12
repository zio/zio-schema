package zio.schema

import zio._
import zio.schema.annotation.validate
import zio.test._
import zio.schema.Schema._
import zio.schema.validation.Validation
import zio.schema.validation.ValidationError

object SchemaValidationSpec extends ZIOSpecDefault {
  import Assertion._

  final case class ExampleData(
    either: scala.util.Either[Person, Person] = Right(goodData),
    option: Option[Person] = None,
    list: List[Person] = goodData :: Nil,
    tuple: scala.Tuple2[Person, Person] = (goodData, goodData),
    // map: Map[String, Person] = Map("foo" -> goodData), // TODO - figure out what todo about Map
    wrapper: Wrapper = Wrapper(goodData),
    enumData: EnumData = EnumData.Case1(goodData)
  )

  sealed trait EnumData

  object EnumData {
    final case class Case1(person: Person) extends EnumData
    final case class Case2(person: Person) extends EnumData
  }

  final case class Wrapper(person: Person)

  implicit val schema: CaseClass6[scala.util.Either[Person,Person],Option[Person],List[Person],(Person, Person),Wrapper,EnumData,ExampleData] = DeriveSchema.gen[ExampleData]

  val badData: Person  = Person("foo", 123123123)
  val goodData: Person = Person("foo", 100)

  final case class Person(name: String, @validate(Validation.between(0, 120)) age: Int)
  // val schemaCaseClass2: Schema.CaseClass2[String, Int, Person] = DeriveSchema.gen[Person]
  val schemaPerson: CaseClass2[String, Int, Person] = DeriveSchema.gen[Person]

  final case class Grade(@validate(Validation.greaterThan(0)) value: Int)
  // val schemaCaseClass1: Schema.CaseClass1[Int, Grade] = DeriveSchema.gen[Grade]
  val schemaGrade: CaseClass1[Int, Grade] = DeriveSchema.gen[Grade]

  override def spec: Spec[Environment, Any] = suite("Schema Validation Spec")(
    test("Invalid CaseClass1 creation") {
      val grade                               = Grade(-50)
      implicit val gradeSchema: Schema[Grade] = schemaGrade
      val validated: Chunk[ValidationError]   = Schema.validate(grade)

      val expected: Chunk[ValidationError] =
        Chunk(ValidationError.GreaterThan(-50, 0))

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Valid CaseClass1 creation") {
      val grade                               = Grade(97)
      implicit val gradeSchema: Schema[Grade] = schemaGrade
      val validated: Chunk[ValidationError]   = Schema.validate(grade)

      val expected: Chunk[ValidationError] = Chunk.empty

      assert(validated)(equalTo(expected))
    },
    test("Invalid CaseClass2 creation") {
      val person                                = Person("Michelle", 200)
      implicit val personSchema: Schema[Person] = schemaPerson
      val validated: Chunk[ValidationError]     = Schema.validate(person)

      val expected: Chunk[ValidationError] =
        Chunk(ValidationError.LessThan(200, 120), ValidationError.EqualTo(200, 120))

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Valid CaseClass2 creation") {
      val person                                = Person("Michelle", 20)
      implicit val personSchema: Schema[Person] = schemaPerson
      val validated: Chunk[ValidationError]     = Schema.validate(person)

      val expected: Chunk[ValidationError] = Chunk.empty

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, no ValidationErrors") {
      val exampleData = ExampleData()

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk.empty

      assert(validated)(hasSameElements(expected))
    },
    test("Example Data, Left ValidationError") {
      val exampleData = ExampleData(either = Left(badData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, Right ValidationError") {
      val exampleData = ExampleData(either = Right(badData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, Option value without ValidationError") {
      val exampleData = ExampleData(option = Some(goodData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk.empty

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, Option value with ValidationError") {
      val exampleData = ExampleData(option = Some(badData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, single element List with ValidationError") {
      val exampleData = ExampleData(list = badData :: Nil)

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, multi element List with ValidationError") {
      val exampleData = ExampleData(list = goodData :: goodData :: badData :: Nil)

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, Tuple with ValidationError on first element") {
      val exampleData = ExampleData(tuple = (badData, goodData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, Tuple with ValidationError on second element") {
      val exampleData = ExampleData(tuple = (goodData, badData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, Wrapper class wrapping class with ValidationError") {
      val exampleData = ExampleData(wrapper = Wrapper(badData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, first Enum with ValidationError") {
      val exampleData = ExampleData(enumData = EnumData.Case1(badData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    test("Example Data, second Enum with ValidationError") {
      val exampleData = ExampleData(enumData = EnumData.Case2(badData))

      val validated: Chunk[ValidationError] = Schema.validate(exampleData)

      val expected: Chunk[ValidationError] = Chunk(
        ValidationError.LessThan(123123123, 120),
        ValidationError.EqualTo(123123123, 120)
      )

      assert(validated)(Assertion.hasSameElements(expected))
    },
    // test("Example Data, Map with ValidationError") {

    //   val exampleData: scala.collection.Map[String, Person] = scala.collection.Map("foo" -> badData)

    //   val keySchema       = zio.schema.Schema.primitive(StandardType.StringType)
    //   val valueSchema     = schemaPerson
    //   implicit val schema = zio.schema.Schema.Map[String, Person](keySchema, valueSchema)

    //   val validated: Chunk[ValidationError] = Schema.validate(exampleData)

    //   val expected: Chunk[ValidationError] = Chunk(
    //     ValidationError.LessThan(123123123, 120),
    //     ValidationError.EqualTo(123123123, 120)
    //   )

    //   assert(validated)(Assertion.hasSameElements(expected))
    // },
    test("Validator successfully extracts from validation annotation") {
      val validation = schemaPerson.field2.validation

      assertTrue(validation.validate(45).isRight) && assertTrue(validation.validate(-20).isLeft)
    }
  )
}
