package dev.zio.schema.example.example9

import zio.schema.{ DeriveSchema, Schema }

object Patching extends App {

  final case class Person(name: String, age: Int)

  object Person {
    implicit lazy val schema: Schema[Person] = DeriveSchema.gen
  }

  val person1 = Person("Gabriel", 45)
  val person2 = Person("Gabi", 54)

  import Person._

  val patch    = schema.diff(person1, person2)
  val inverted = patch.invert

  val result1 = patch.patch(person1)
  val result2 = result1.flatMap(inverted.patch)

  assert(result2 == Right(person1))

}
