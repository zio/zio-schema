package zio.schema

import scala.annotation.nowarn

import zio.test._

trait VersionSpecificDeriveSchemaSpec extends ZIOSpecDefault {
  case class ContainerFields(field1: Option[String])

  object ContainerFields {
    implicit val schema: Schema.CaseClass1.WithFields["field1", Option[String], ContainerFields] =
      DeriveSchema.gen[ContainerFields]
  }

  def verifyFieldName[F]: FieldNameVerifier[F] = new FieldNameVerifier[F]

  class FieldNameVerifier[F] {
    @nowarn def apply[S <: String & scala.Singleton](name: S)(implicit ev: F =:= S): Boolean = true
  }

  def versionSpecificSuite: Spec[Any, Nothing] = Spec.labeled("Scala 2 specific tests", Spec.empty)
}
