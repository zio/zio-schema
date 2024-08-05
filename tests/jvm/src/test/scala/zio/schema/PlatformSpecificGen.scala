package zio.schema

import zio.schema.SchemaGen.SchemaTest
import zio.schema.StandardTypeGen.StandardTypeAndGen
import zio.test.Gen

object PlatformSpecificGen {

  val platformSpecificStandardTypes: Gen[Any, StandardType[Any]] = Gen.fromIterable(
    List(
      StandardType.CurrencyType
    ).map(_.asInstanceOf[StandardType[Any]])
  )

  def platformSpecificStandardTypeAndGen(standardTypeGen: StandardType[Any]): StandardTypeAndGen[Any] =
    standardTypeGen match {
      case typ if typ.isInstanceOf[StandardType.CurrencyType.type] => typ                                                   -> Gen.currency
      case _                                                       => StandardType.UnitType.asInstanceOf[StandardType[Any]] -> Gen.unit
    }

  val platformSpecificSchemasAndGens: List[SchemaTest[Any]] = List(
    SchemaTest("Currency", StandardType.CurrencyType.asInstanceOf[StandardType[Any]], Gen.currency)
  )
}
