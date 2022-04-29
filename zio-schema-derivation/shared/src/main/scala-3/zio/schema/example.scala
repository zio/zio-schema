package zio.schema

object UseMacros {

  // val personSchema = DeriveSchema.gen[Person]
  // implicit val address   = DeriveSchema.gen[Address]
  // val schema      = DeriveSchema.gen[PersonWithAddress]
  // val colorSchema = DeriveSchema.gen[Color]
  val i = 12

  @main
  def cool = {
    println("CYLCE!")
    println("CYLCE!")
    println("CYLCE!")
    // println(Cyclic.schema)
    // println("PERSON SCHEMA!")
    // println(personSchema)
    // println(personSchema)
    // println(personSchema.annotations)
    // println(personSchema.field1.annotations)
    // println("")
    // val person = personSchema.construct(PersonId(123),"Kit", 31, true, List.empty)
    // println(person)
    // println(personSchema.extractField1(person))
    // println(personSchema.extractField2(person))

    // println("\nCOLOR SCHEMA")
    // println(schema)
    // val color = Color.Blue
    // println(colorSchema.case1.unsafeDeconstruct(color))
  }
}
