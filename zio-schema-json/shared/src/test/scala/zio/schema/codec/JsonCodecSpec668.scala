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

  // GeoJSON-inspired ADT with leaf classes at different hierarchy levels.
  // Level 1 leaves: Feature, FeatureCollection (directly extend GeoJSON)
  // Level 2 leaves: Point, LineString, Polygon (extend Geometry which extends GeoJSON)
  // This exercises the fix for hierarchical enums with non-trivial data fields.
  sealed trait GeoJSON

  object GeoJSON {
    sealed trait Geometry extends GeoJSON

    object Geometry {
      final case class Point(coordinates: (Double, Double))               extends Geometry
      final case class LineString(coordinates: List[(Double, Double)])    extends Geometry
      final case class Polygon(coordinates: List[List[(Double, Double)]]) extends Geometry

      implicit lazy val schema: Schema[Geometry] = DeriveSchema.gen[Geometry]
    }

    final case class Feature(
      geometry: Option[Geometry],
      properties: Map[String, String]
    ) extends GeoJSON

    final case class FeatureCollection(
      features: List[Feature]
    ) extends GeoJSON

    implicit lazy val schema: Schema[GeoJSON] = DeriveSchema.gen[GeoJSON]
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
    },
    suite("GeoJSON multi-level hierarchy")(
      test("should round-trip level-2 leaf: Point") {
        val codec          = JsonCodec.jsonCodec(GeoJSON.schema)
        val value: GeoJSON = GeoJSON.Geometry.Point((102.0, 0.5))

        val encoded = codec.encoder.encodeJson(value, None)
        val decoded = codec.decoder.decodeJson(encoded)

        assertTrue(decoded == Right(value))
      },
      test("should round-trip level-2 leaf: LineString") {
        val codec          = JsonCodec.jsonCodec(GeoJSON.schema)
        val value: GeoJSON = GeoJSON.Geometry.LineString(List((102.0, 0.0), (103.0, 1.0), (104.0, 0.0)))

        val encoded = codec.encoder.encodeJson(value, None)
        val decoded = codec.decoder.decodeJson(encoded)

        assertTrue(decoded == Right(value))
      },
      test("should round-trip level-2 leaf: Polygon") {
        val codec = JsonCodec.jsonCodec(GeoJSON.schema)
        val value: GeoJSON = GeoJSON.Geometry.Polygon(
          List(List((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (0.0, 0.0)))
        )

        val encoded = codec.encoder.encodeJson(value, None)
        val decoded = codec.decoder.decodeJson(encoded)

        assertTrue(decoded == Right(value))
      },
      test("should round-trip level-1 leaf: Feature with geometry") {
        val codec = JsonCodec.jsonCodec(GeoJSON.schema)
        val value: GeoJSON = GeoJSON.Feature(
          Some(GeoJSON.Geometry.Point((102.0, 0.5))),
          Map("name" -> "test")
        )

        val encoded = codec.encoder.encodeJson(value, None)
        val decoded = codec.decoder.decodeJson(encoded)

        assertTrue(decoded == Right(value))
      },
      test("should round-trip level-1 leaf: Feature without geometry") {
        val codec          = JsonCodec.jsonCodec(GeoJSON.schema)
        val value: GeoJSON = GeoJSON.Feature(None, Map.empty)

        val encoded = codec.encoder.encodeJson(value, None)
        val decoded = codec.decoder.decodeJson(encoded)

        assertTrue(decoded == Right(value))
      },
      test("should round-trip level-1 leaf: FeatureCollection") {
        val codec = JsonCodec.jsonCodec(GeoJSON.schema)
        val value: GeoJSON = GeoJSON.FeatureCollection(
          List(
            GeoJSON.Feature(Some(GeoJSON.Geometry.Point((1.0, 2.0))), Map("a" -> "b")),
            GeoJSON.Feature(Some(GeoJSON.Geometry.LineString(List((0.0, 0.0), (1.0, 1.0)))), Map.empty)
          )
        )

        val encoded = codec.encoder.encodeJson(value, None)
        val decoded = codec.decoder.decodeJson(encoded)

        assertTrue(decoded == Right(value))
      },
      test("should round-trip all GeoJSON variants and re-encode stably") {
        val codec = JsonCodec.jsonCodec(GeoJSON.schema)

        val cases: List[GeoJSON] = List(
          GeoJSON.Geometry.Point((1.0, 2.0)),
          GeoJSON.Geometry.LineString(List((0.0, 0.0), (1.0, 1.0))),
          GeoJSON.Geometry.Polygon(List(List((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 0.0)))),
          GeoJSON.Feature(Some(GeoJSON.Geometry.Point((3.0, 4.0))), Map("key" -> "val")),
          GeoJSON.Feature(None, Map.empty),
          GeoJSON.FeatureCollection(List(GeoJSON.Feature(None, Map.empty)))
        )

        val results = cases.map { value =>
          val json      = codec.encoder.encodeJson(value, None)
          val decoded   = codec.decoder.decodeJson(json)
          val reencoded = decoded.map(r => codec.encoder.encodeJson(r, None))
          (decoded == Right(value), reencoded == Right(json))
        }

        assertTrue(results.forall { case (d, r) => d && r })
      }
    )
  )
}
