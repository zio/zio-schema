package zio.schema

import zio.Chunk
import zio.test.*
import zio.schema.annotation.*

trait VersionSpecificDeriveSchemaSpec extends ZIOSpecDefault {
  /** ObjectWithDoc doc */
  object ObjectWithDoc

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

  /** AutoDerives scaladoc */
  final case class AutoDerivesWithDoc(i: Int) derives Schema

  enum Colour(val rgb: Int) {
    case Red extends Colour(0xff0000)
    case Green extends Colour(0x00ff00)
    case Blue extends Colour(0x0000ff)
  }

  /** Colour scaladoc */
  @caseName("Red")
  enum ColourWithDoc(val rgb: Int) {
    /** Red scaladoc */
    case Red extends ColourWithDoc(0xff0000)
    case Green extends ColourWithDoc(0x00ff00)
    case Blue extends ColourWithDoc(0x0000ff)
  }

  @description("Colour Enum")
  enum ColourAnnotations:
    @description("Red")
    case Red

  enum NonSimpleEnum1:
    case A(a: Int)

  enum NonSimpleEnum2(a: Int):
    case A(b: Int) extends NonSimpleEnum2(0)


  enum NonSimpleEnum3(a: Int):
    case A(b: Int) extends NonSimpleEnum3(b)

  enum NonSimpleEnum4(val a: Int):
    case A(override val a: Int) extends NonSimpleEnum4(a)

  enum NonSimpleEnum5(a: Int, b: String):
    case A extends NonSimpleEnum5(0, "")
    case B(n: Int) extends NonSimpleEnum5(n, "")

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
      test("correctly assigns simpleEnum to enum") {
        val derived: Schema[Colour] = DeriveSchema.gen[Colour]
        assertTrue(derived.annotations == Chunk(simpleEnum(true)))
      },
      test("doesn't assigns simpleEnum to non-simple enum") {
        val derived1: Schema[NonSimpleEnum1] = DeriveSchema.gen[NonSimpleEnum1]
        val derived2: Schema[NonSimpleEnum2] = DeriveSchema.gen[NonSimpleEnum2]
        val derived3: Schema[NonSimpleEnum3] = DeriveSchema.gen[NonSimpleEnum3]
        val derived4: Schema[NonSimpleEnum4] = DeriveSchema.gen[NonSimpleEnum4]
        val derived5: Schema[NonSimpleEnum5] = DeriveSchema.gen[NonSimpleEnum5]
        assertTrue(derived1.annotations.isEmpty) &&
        assertTrue(derived2.annotations.isEmpty) &&
        assertTrue(derived3.annotations.isEmpty) &&
        assertTrue(derived4.annotations.isEmpty) &&
        assertTrue(derived5.annotations.isEmpty)
      },
      test("derive different annotations for parent and child in enum") {
        val parent = DeriveSchema.gen[ColourAnnotations]
        val child = parent match {
          case Schema.Enum1(_, c, _) => c
        }
        assertTrue(child.annotations == Chunk(description("Red"))) &&
        assertTrue(parent.annotations == Chunk(simpleEnum(true), description("Colour Enum")))
      },
      test("correctly derive annotations for child in enum") {
        val child: Schema[ColourAnnotations.Red.type] = DeriveSchema.gen[ColourAnnotations.Red.type]
        assertTrue(child.annotations == Chunk(description("Red")))
      },
      test("correctly adds scaladoc as description"){
        val colourWithDoc: Schema[ColourWithDoc] = DeriveSchema.gen[ColourWithDoc]
        val autoDerivesWithDoc: Schema[AutoDerivesWithDoc] = Schema[AutoDerivesWithDoc]
        val objectWithDoc: Schema[ObjectWithDoc.type] = DeriveSchema.gen[ObjectWithDoc.type]
        val redAnnotations = colourWithDoc.asInstanceOf[Schema.Enum[ColourWithDoc]].cases.find(_.id == "Red").get.schema.annotations.find(_.isInstanceOf[description])
        assertTrue(
          colourWithDoc.annotations.find(_.isInstanceOf[description]) == Some(description("/** Colour scaladoc */")),
          redAnnotations == Some(description("/** Red scaladoc */")),
          autoDerivesWithDoc.annotations.find(_.isInstanceOf[description]) == Some(description("/** AutoDerives scaladoc */")),
          objectWithDoc.annotations.find(_.isInstanceOf[description]) == Some(description("/** ObjectWithDoc doc */")),
          )
      },
    )
  )
}
