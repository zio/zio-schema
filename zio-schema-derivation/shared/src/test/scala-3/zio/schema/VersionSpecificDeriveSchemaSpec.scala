package zio.schema

import zio.test.*

trait VersionSpecificDeriveSchemaSpec extends ZIOSpecDefault {
  case class ContainerFields(field1: Option[String])

  object ContainerFields {
    transparent inline given Schema[ContainerFields] = DeriveSchema.gen[ContainerFields]
  }

  inline def verifyFieldName[F]: FieldNameVerifier[F] = new FieldNameVerifier[F]
    
    
  class FieldNameVerifier[F] {
     inline def apply[S <: String & scala.Singleton](name: S): Boolean =
       VerifyFieldNameMacro.verifyFieldName[F, S]
  }

  def versionSpecificSuite = Spec.labeled("Scala 3 specific tests", Spec.empty)
}
