package zio.schema

object UseMacros {

  // implicit val schema    = DeriveSchema.gen[Person]
  // implicit val address   = DeriveSchema.gen[Address]
  val schema      = DeriveSchema.gen[PersonWithAddress]
  val colorSchema = DeriveSchema.gen[Color]

  @main
  def cool = {
    println("PERSON WITH ADDRESS SCHEMA")
    println(schema)
    val person = schema.construct(Person("Kit", 31, true), Address("123 Main St", "Anytown", "CA", 90210), 12)
    println(person)
    println(schema.extractField1(person))
    println(schema.extractField2(person))

    println("\nCOLOR SCHEMA")
    println(schema)
    // val color = Color.Blue
    // println(colorSchema.case1.unsafeDeconstruct(color))
  }
}
