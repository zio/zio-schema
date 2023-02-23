package zio.schema

import zio.test._

trait VersionSpecificDeriveSpec extends ZIOSpecDefault {

  def versionSpecificSuite: Spec[Any, Nothing] = Spec.labeled("Scala 2 specific tests", Spec.empty)
}
