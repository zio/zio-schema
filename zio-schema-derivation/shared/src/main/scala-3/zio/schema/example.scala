package zio.schema

object UseMacros {

  // implicit val schema    = DeriveSchema.gen[Person]
  // implicit val address   = DeriveSchema.gen[Address]
  val schema = DeriveSchema.gen[PersonWithAddress]

  @main
  def cool = {
    println("SCHEMA")
    println(schema)
    val person = schema.construct(Person("Kit", 31, true), Address("123 Main St", "Anytown", "CA", 90210))
    println(person)
    println(schema.extractField1(person))
    println(schema.extractField2(person))
  }
}
