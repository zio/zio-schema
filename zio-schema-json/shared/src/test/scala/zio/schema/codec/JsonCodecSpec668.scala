package zio.schema.codec

import zio.schema._
import zio.test._

/**
 * Regression test for issue #668:
 * JSON codec built from auto-derived schema fails for enumeration with intermediate type
 *
 * The bug occurred when encoding hierarchical sealed trait enums where intermediate
 * types exist (e.g., Animal > Mammal > Bison). The JsonCodec was incorrectly assuming
 * all enum cases have CaseClass0 schemas, but intermediate sealed traits have Enum schemas.
 */
object JsonCodecSpec668 extends ZIOSpecDefault {

  // Reproducer from issue #668
  sealed trait Animal

  object Animal {
    sealed trait Mammal extends Animal
    case object Bison   extends Mammal

    implicit val schema: Schema[Animal] = DeriveSchema.gen[Animal]
  }

  // Additional test case with deeper hierarchy
  sealed trait Vehicle

  object Vehicle {
    sealed trait LandVehicle extends Vehicle
    sealed trait Car         extends LandVehicle
    case object Sedan        extends Car
    case object Truck        extends LandVehicle

    implicit val schema: Schema[Vehicle] = DeriveSchema.gen[Vehicle]
  }

  override def spec: Spec[Any, Nothing] = suite("JsonCodec Issue #668 - Hierarchical Enums")(
    test("should encode case object from intermediate sealed trait") {
      val codec         = JsonCodec.jsonCodec(Animal.schema)
      val value: Animal = Animal.Bison

      val encoded = codec.encoder.encodeJson(value, None)
      val decoded = codec.decoder.decodeJson(encoded)

      assertTrue(decoded == Right(value))
    },
    test("should encode deeply nested enum hierarchy") {
      val codec          = JsonCodec.jsonCodec(Vehicle.schema)
      val sedan: Vehicle = Vehicle.Sedan
      val truck: Vehicle = Vehicle.Truck

      val encodedSedan = codec.encoder.encodeJson(sedan, None)
      val decodedSedan = codec.decoder.decodeJson(encodedSedan)
      val encodedTruck = codec.encoder.encodeJson(truck, None)
      val decodedTruck = codec.decoder.decodeJson(encodedTruck)

      assertTrue(
        decodedSedan == Right(sedan),
        decodedTruck == Right(truck)
      )
    },
    test("should handle round-trip encoding/decoding") {
      val codec         = JsonCodec.jsonCodec(Animal.schema)
      val value: Animal = Animal.Bison

      val json      = codec.encoder.encodeJson(value, None)
      val result    = codec.decoder.decodeJson(json)
      val reencoded = result.map(r => codec.encoder.encodeJson(r, None))

      assertTrue(
        result == Right(value),
        reencoded == Right(json)
      )
    }
  )
}
