package zio.schema

case class Person(
  name: String,
  age: Int
)

// object Person {
//   val schema = DeriveSchema.gen[Person]
// }
