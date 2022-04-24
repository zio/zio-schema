package zio.schema

object UseMacros {

  val schema = DeriveSchema.gen[Person]

  @main
  def cool = {
    println("SCHEMA")
    val person = schema.construct("Hello", 12)
    println(person)
    println(schema.extractField1(person))
    println(schema.extractField2(person))
  }
}
