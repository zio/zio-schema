package dev.zio.schema.example.example9

import zio.schema.{ DeriveSchema, Patch, Schema }

object Patching extends App {

  final case class Person(name: String, age: Int)

  object Person {
    implicit lazy val schema: Schema[Person] = DeriveSchema.gen
  }

  private val person1 = Person("Gabriel", 45)
  private val person2 = Person("Gabi", 54)

  import Person._

  private val patch: Patch[Person]    = schema.diff(person1, person2)
  private val inverted: Patch[Person] = patch.invert

  private val result1: Either[String, Person] = patch.patch(person1)
  private val result2: Either[String, Person] = result1.flatMap(inverted.patch)

  assert(result2 == Right(person1))

}
