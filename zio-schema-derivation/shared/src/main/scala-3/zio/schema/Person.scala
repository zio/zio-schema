package zio.schema

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
  address: Address
)
// object Person {
//   val schema = DeriveSchema.gen[Person]
// }
