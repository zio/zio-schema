package zio.schema

import scala.annotation.StaticAnnotation

enum Color {
  case Red
  case Green
  case Blue
}

case class name(string: String) extends StaticAnnotation

@name("kit")
case class Person(
  @name("wow")
  @name("name field")
  name: String,
  @name("person age")
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
