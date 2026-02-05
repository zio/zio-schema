package zio.schema

import zio.Chunk
import zio.schema.validation.Validation
import zio.test._

object Issue511Spec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] = suite("Issue511Spec")(
    test("toTypedValue works with registry after AST reconstruction") {
      final case class User(name: String, age: Int)
      implicit val schema: Schema[User] = Schema.CaseClass2[String, Int, User](
        TypeId.parse("zio.schema.Issue511Spec.User"),
        Schema.Field("name", Schema[String], Chunk.empty, Validation.succeed, _.name, (u, n) => u.copy(name = n)),
        Schema.Field("age", Schema[Int], Chunk.empty, Validation.succeed, _.age, (u, a) => u.copy(age = a)),
        (name, age) => User(name, age)
      )

      val user         = User("John", 30)
      val dynamicValue = schema.toDynamic(user)

      val registry  = Map(schema.asInstanceOf[Schema.Record[User]].id -> schema)
      val astSchema = schema.ast.toSchema(registry)

      val result = dynamicValue.toTypedValue(astSchema)

      assertTrue(result == Right(user))
    },
    test("nested product test - Company with Department with Manager") {
      final case class Manager(name: String, salary: Int)
      val managerSchema: Schema[Manager] = Schema.CaseClass2[String, Int, Manager](
        TypeId.parse("zio.schema.Issue511Spec.Manager"),
        Schema.Field("name", Schema[String], Chunk.empty, Validation.succeed, _.name, (m, n) => m.copy(name = n)),
        Schema.Field("salary", Schema[Int], Chunk.empty, Validation.succeed, _.salary, (m, s) => m.copy(salary = s)),
        (name, salary) => Manager(name, salary)
      )

      final case class Department(name: String, manager: Manager)
      val departmentSchema: Schema[Department] = Schema.CaseClass2[String, Manager, Department](
        TypeId.parse("zio.schema.Issue511Spec.Department"),
        Schema.Field("name", Schema[String], Chunk.empty, Validation.succeed, _.name, (d, n) => d.copy(name = n)),
        Schema
          .Field("manager", managerSchema, Chunk.empty, Validation.succeed, _.manager, (d, m) => d.copy(manager = m)),
        (name, manager) => Department(name, manager)
      )

      final case class Company(name: String, department: Department)
      val companySchema: Schema[Company] = Schema.CaseClass2[String, Department, Company](
        TypeId.parse("zio.schema.Issue511Spec.Company"),
        Schema.Field("name", Schema[String], Chunk.empty, Validation.succeed, _.name, (c, n) => c.copy(name = n)),
        Schema.Field(
          "department",
          departmentSchema,
          Chunk.empty,
          Validation.succeed,
          _.department,
          (c, d) => c.copy(department = d)
        ),
        (name, department) => Company(name, department)
      )

      val company      = Company("TechCorp", Department("Engineering", Manager("Alice", 100000)))
      val dynamicValue = companySchema.toDynamic(company)

      val registry = Map(
        managerSchema.asInstanceOf[Schema.Record[Manager]].id       -> managerSchema,
        departmentSchema.asInstanceOf[Schema.Record[Department]].id -> departmentSchema,
        companySchema.asInstanceOf[Schema.Record[Company]].id       -> companySchema
      )
      val astSchema = companySchema.ast.toSchema(registry)

      val result = dynamicValue.toTypedValue(astSchema)

      assertTrue(result == Right(company))
    },
    test("recursive test - Category with subCategories") {
      final case class Category(name: String, subCategories: List[Category])

      implicit lazy val categorySchema: Schema[Category] = Schema.defer {
        Schema.CaseClass2[String, List[Category], Category](
          TypeId.parse("zio.schema.Category"),
          Schema.Field(
            "name",
            Schema[String],
            Chunk.empty,
            Validation.succeed,
            (c: Category) => c.name,
            (c: Category, n: String) => c.copy(name = n)
          ),
          Schema.Field(
            "subCategories",
            Schema[List[Category]],
            Chunk.empty,
            Validation.succeed,
            (c: Category) => c.subCategories,
            (c: Category, s: List[Category]) => c.copy(subCategories = s)
          ),
          (name: String, subCategories: List[Category]) => Category(name, subCategories)
        )
      }

      val category     = Category("Electronics", List(Category("Phones", Nil), Category("Laptops", Nil)))
      val dynamicValue = categorySchema.toDynamic(category)

      val result = dynamicValue.toTypedValue(categorySchema)

      assertTrue(result == Right(category))
    },
    test("enum-in-record test - Person with Status enum") {
      sealed trait Status
      object Status {
        case object Active   extends Status
        case object Inactive extends Status
        case object Pending  extends Status
      }

      val activeCase = Schema.Case[Status, Unit](
        "Active",
        Schema.CaseClass0[Unit](TypeId.parse("zio.schema.Issue511Spec.Status.Active"), () => ()),
        (_: Status) => (),
        (_: Unit) => Status.Active,
        (_: Status) == Status.Active,
        Chunk.empty
      )
      val inactiveCase = Schema.Case[Status, Unit](
        "Inactive",
        Schema.CaseClass0[Unit](TypeId.parse("zio.schema.Issue511Spec.Status.Inactive"), () => ()),
        (_: Status) => (),
        (_: Unit) => Status.Inactive,
        (_: Status) == Status.Inactive,
        Chunk.empty
      )
      val pendingCase = Schema.Case[Status, Unit](
        "Pending",
        Schema.CaseClass0[Unit](TypeId.parse("zio.schema.Issue511Spec.Status.Pending"), () => ()),
        (_: Status) => (),
        (_: Unit) => Status.Pending,
        (_: Status) == Status.Pending,
        Chunk.empty
      )

      val caseSet =
        CaseSet.Cons(activeCase, CaseSet.Cons(inactiveCase, CaseSet.Cons(pendingCase, CaseSet.Empty[Status]())))
      val statusSchema: Schema[Status] = Schema.enumeration(TypeId.parse("zio.schema.Issue511Spec.Status"), caseSet)

      final case class Person(name: String, status: Status)
      val personSchema: Schema[Person] = Schema.CaseClass2[String, Status, Person](
        TypeId.parse("zio.schema.Issue511Spec.Person"),
        Schema.Field("name", Schema[String], Chunk.empty, Validation.succeed, _.name, (p, n) => p.copy(name = n)),
        Schema.Field("status", statusSchema, Chunk.empty, Validation.succeed, _.status, (p, s) => p.copy(status = s)),
        (name, status) => Person(name, status)
      )

      val person       = Person("Bob", Status.Active)
      val dynamicValue = personSchema.toDynamic(person)

      val registry = Map(
        statusSchema.asInstanceOf[Schema.Enum[Status]].id        -> statusSchema,
        personSchema.asInstanceOf[Schema.Record[Person]].id      -> personSchema,
        activeCase.schema.asInstanceOf[Schema.Record[Unit]].id   -> activeCase.schema,
        inactiveCase.schema.asInstanceOf[Schema.Record[Unit]].id -> inactiveCase.schema,
        pendingCase.schema.asInstanceOf[Schema.Record[Unit]].id  -> pendingCase.schema
      )

      val astSchema = personSchema.ast.toSchema(registry)
      val result    = dynamicValue.toTypedValue(astSchema)

      assertTrue(result == Right(person))
    },
    test("handle registry poisoning gracefully") {
      final case class Person(name: String, age: Int)
      val personSchema: Schema[Person] = Schema.CaseClass2[String, Int, Person](
        TypeId.parse("zio.schema.Issue511Spec.Person"),
        Schema.Field(
          "name",
          Schema[String],
          Chunk.empty,
          Validation.succeed,
          (p: Person) => p.name,
          (p: Person, n: String) => p.copy(name = n)
        ),
        Schema.Field(
          "age",
          Schema[Int],
          Chunk.empty,
          Validation.succeed,
          (p: Person) => p.age,
          (p: Person, a: Int) => p.copy(age = a)
        ),
        (name: String, age: Int) => Person(name, age)
      )

      val person       = Person("Alice", 30)
      val dynamicValue = personSchema.toDynamic(person)

      val poisonedRegistry = Map(
        TypeId.parse("zio.schema.Issue511Spec.Person") -> Schema.Fail("Mismatched schema")
      )

      val astSchema = personSchema.ast.toSchema(poisonedRegistry)
      val result    = dynamicValue.toTypedValue(astSchema)

      assertTrue(
        result.isRight && result.toOption.get.isInstanceOf[scala.collection.immutable.ListMap[_, _]]
      )
    }
  )
}
