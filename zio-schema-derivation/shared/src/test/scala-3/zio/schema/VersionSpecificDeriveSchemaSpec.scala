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

  import SchemaAssertions._

  final case class AutoDerives(i: Int) derives Schema

  def versionSpecificSuite = Spec.labeled(
    "Scala 3 specific tests", 
    suite("Derivation")(
      test("correctly derives case class with `derives` keyword") {
        val expected: Schema[AutoDerives] = Schema.CaseClass1(
          TypeId.parse("zio.schema.VersionSpecificDeriveSchemaSpec.AutoDerives"),
          field0 = Schema.Field(
            "i",
            Schema.Primitive(StandardType.IntType),
            get0 = _.i,
            set0 = (a, b: Int) => a.copy(i = b)
          ),
          AutoDerives.apply
        )
        assert(Schema[AutoDerives])(hasSameSchema(expected))
      },
      test("correctly assign simpleEnum to enum"){
        enum Colour(val rgb: Int):
          case Red extends Colour(0xff0000)
          case Green extends Colour(0x00ff00)
          case Blue extends Colour(0x0000ff)
        

        val derived = DeriveSchema.gen[Colour]
        assertTrue(derived.annotations == Chunk(simpleEnum(true)))
      }
    )
  )
}
