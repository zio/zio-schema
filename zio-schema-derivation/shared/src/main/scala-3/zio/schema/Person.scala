package zio.schema

enum Color {
  case Red
  case Green
  case Blue
}

case class Person(
  name: String,
  age: Int,
  isAlive: Boolean
)

case class Address(
  street: String,
  city: String,
  state: String,
  zip: Int
)

case class PersonWithAddress(
  person: Person,
  address: Address,
  meta: Int
)
// object Person {
//   val schema = DeriveSchema.gen[Person]
// }
