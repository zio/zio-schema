package zio.schema

import scala.annotation.nowarn

import zio.test._

trait VersionSpecificDeriveSchemaSpec {
  case class ContainerFields(field1: Option[String])

  object ContainerFields {
    implicit val schema = DeriveSchema.gen[ContainerFields]
  }

  def verifyFieldName[F]: FieldNameVerifier[F] = new FieldNameVerifier[F]

  class FieldNameVerifier[F] {
    @nowarn def apply(name: String): Boolean = true // Cannot check as we don't have singleton types
  }

  def versionSpecificSuite: Spec[Any, Nothing] = Spec.labeled("Scala 2.12 specific tests", Spec.empty)
}
