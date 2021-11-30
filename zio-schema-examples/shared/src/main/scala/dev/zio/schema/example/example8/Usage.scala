package dev.zio.schema.example.example8

import zio.schema.{DeriveSchema, Schema}

final case class Person(name: String, age: Double)
object Person {
  implicit val schemaPerson: Schema[Person] = DeriveSchema.gen[Person]
}

object Usage extends App {
  val p = Person("cal", 30)

  val pJson = Encoder.deriveEncoder[Person].encode(p)
  val sameP = Decoder.deriveDecoder[Person].decode(pJson)
  println(sameP == Right(p))

  println {
    Decoder
      .deriveDecoder[Person]
      .decode(
        Json.JObj(
          Map(
            "name" -> Json.JStr("cal"),
            "age"  -> Json.JNum(30)
          )
        )
      )
  }
}
