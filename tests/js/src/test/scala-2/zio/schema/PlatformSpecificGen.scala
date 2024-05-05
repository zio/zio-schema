package zio.schema

import zio.schema.SchemaGen.SchemaTest
import zio.schema.StandardTypeGen.StandardTypeAndGen
import zio.test.Gen

object PlatformSpecificGen {

  val platformSpecificStandardTypes: Gen[Any, StandardType[_]] = Gen.fromIterable(
    List.empty
  )

  def platformSpecificStandardTypeAndGen(standardTypeGen: StandardType[_]): StandardTypeAndGen[_] =
    standardTypeGen match {
      case _ => StandardType.UnitType -> Gen.unit: StandardTypeAndGen[_]
    }

  val platformSpecificSchemasAndGens: List[SchemaTest[_]] = List.empty
}
