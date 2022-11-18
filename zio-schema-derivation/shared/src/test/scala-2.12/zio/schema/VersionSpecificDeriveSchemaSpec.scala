package zio.schema

import scala.annotation.nowarn

trait VersionSpecificDeriveSchemaSpec {
  case class ContainerFields(field1: Option[String])

  object ContainerFields {
    implicit val schema = DeriveSchema.gen[ContainerFields]
  }

  def verifyFieldName[F]: FieldNameVerifier[F] = new FieldNameVerifier[F]

  class FieldNameVerifier[F] {
    @nowarn def apply(name: String): Boolean = true // Cannot check as we don't have singleton types
  }
}
