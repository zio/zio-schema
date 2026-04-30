package zio.schema

import zio._
import zio.test._

object AlgebraicMigrationSpec extends ZIOSpecDefault {
  def spec = suite("AlgebraicMigrationSpec")(
    test("AddField migration") {
      val schemaA = Schema.CaseClass1[String, PersonA](
        TypeId.parse("PersonA"),
        Schema.Field("name", Schema.primitive[String], get0 = _.name, set0 = (p, n) => p.copy(name = n)),
        PersonA(_)
      )
      val schemaB = Schema.CaseClass2[String, Int, PersonB](
        TypeId.parse("PersonB"),
        Schema.Field("name", Schema.primitive[String], get0 = _.name, set0 = (p, n) => p.copy(name = n)),
        Schema.Field("age", Schema.primitive[Int], get0 = _.age, set0 = (p, a) => p.copy(age = a)),
        (name, age) => PersonB(name, age)
      )

      val migration = new MigrationBuilder(schemaA, schemaB, Chunk.empty)
        .addField(DynamicOptic.Root / "name", SchemaExpr.Constant("Alice", Schema.primitive[String]))
        .addField(DynamicOptic.Root / "age", SchemaExpr.Constant(18, Schema.primitive[Int]))
        .build

      val result = migration.apply(PersonA("Bob"))
      assertTrue(result == Right(PersonB("Alice", 18))) // Alice because of AddField on name too
    },
    test("Rename migration") {
      val schemaA = Schema.CaseClass1[String, PersonA](
        TypeId.parse("PersonA"),
        Schema.Field("name", Schema.primitive[String], get0 = _.name, set0 = (p, n) => p.copy(name = n)),
        PersonA(_)
      )
      val schemaC = Schema.CaseClass1[String, PersonC](
        TypeId.parse("PersonC"),
        Schema.Field("fullName", Schema.primitive[String], get0 = _.fullName, set0 = (p, n) => p.copy(fullName = n)),
        PersonC(_)
      )

      val migration = new MigrationBuilder(schemaA, schemaC, Chunk.empty)
        .renameField(DynamicOptic.Root / "name", "fullName")
        .build

      val result = migration.apply(PersonA("Alice"))
      assertTrue(result == Right(PersonC("Alice")))
    },
    test("Identity Law") {
      val schema = Schema.primitive[Int]
      val migration = Migration.identity[Int](schema)
      assertTrue(migration.apply(1) == Right(1))
    },
    test("Associativity Law") {
      val schema = Schema.primitive[Int]
      val m1 = new MigrationBuilder(schema, schema, Chunk.empty).transformField(DynamicOptic.Root, SchemaExpr.Constant(10, Schema.primitive[Int])).build
      val m2 = new MigrationBuilder(schema, schema, Chunk.empty).transformField(DynamicOptic.Root, SchemaExpr.Constant(20, Schema.primitive[Int])).build
      val m3 = new MigrationBuilder(schema, schema, Chunk.empty).transformField(DynamicOptic.Root, SchemaExpr.Constant(30, Schema.primitive[Int])).build

      val left = (m1 ++ m2) ++ m3
      val right = m1 ++ (m2 ++ m3)

      assertTrue(left.apply(1) == Right(30))
      assertTrue(right.apply(1) == Right(30))
    }
  )

  case class PersonA(name: String)
  case class PersonB(name: String, age: Int)
  case class PersonC(fullName: String)
}
