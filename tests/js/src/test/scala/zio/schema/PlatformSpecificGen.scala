package zio.schema

import zio.schema.SchemaGen.SchemaTest
import zio.schema.StandardTypeGen.StandardTypeAndGen
import zio.test.Gen

object PlatformSpecificGen {

  val platformSpecificStandardTypes: Gen[Any, StandardType[Any]] = Gen.fromIterable(
    List.empty
  )

  def platformSpecificStandardTypeAndGen(standardTypeGen: StandardType[Any]): StandardTypeAndGen[Any] =
    standardTypeGen match {
      case _ => StandardType.UnitType.asInstanceOf[StandardType[Any]] -> Gen.unit
    }

  val platformSpecificSchemasAndGens: List[SchemaTest[Any]] = List.empty
}
