package zio.schema

import zio.schema.Schema._
import zio.schema.SchemaGen.Json.schema
import zio.test._

object AccessorBuilderSpec extends DefaultRunnableSpec {
  import TestAccessorBuilder._
  import Assertion._

  private val builder: TestAccessorBuilder = new TestAccessorBuilder

  override def spec: ZSpec[Environment, Failure] = suite("AccessorBuilder")(
    test("fail") {
      assert(Schema.fail("error").makeAccessors(builder).asInstanceOf[Unit])(isUnit)
    },
    testM("primitive") {
      check(SchemaGen.anyPrimitive) { schema =>
        assert(schema.makeAccessors(builder))(isUnit)
      }
    },
    testM("sequence") {
      check(SchemaGen.anySchema) { elementSchema =>
        val collectionSchema = elementSchema.repeated
        val traversal        = collectionSchema.makeAccessors(builder)

        assert(
          traversal match {
            case Traversal(col, elem) =>
              col == collectionSchema && elem == elementSchema
            case _ => false
          }
        )(isTrue)
      }
    },
    testM("transform") {
      check(SchemaGen.anyPrimitive) { schema =>
        val transform = schema.transformOrFail[Unit](_ => Left("error"), _ => Left("error"))

        val transformAccessor: Any = transform.makeAccessors(builder).asInstanceOf[Any]
        val schemaAccessor: Any    = schema.makeAccessors(builder).asInstanceOf[Any]

        assert(
          transformAccessor == schemaAccessor
        )(isTrue)
      }
    },
    testM("optional") {
      check(SchemaGen.anyPrimitive) { schema =>
        val optionalSchema: Schema.Optional[_] = schema.optional.asInstanceOf[Schema.Optional[_]]
        val enumSchema                         = optionalSchema.toEnum
        val accessor                           = optionalSchema.makeAccessors(builder)

        assert(
          accessor match {
            case (
                Prism(e1, Case("Some", c1, _, _)),
                Prism(e2, Case("None", _, _, _))
                ) =>
              e1 == e2 && e2 == enumSchema && c1 == optionalSchema.someCodec
            case _ => false
          }
        )(isTrue)
      }
    },
    testM("tuple") {
      check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
        case (leftSchema, rightSchema) =>
          val tupleSchema: Schema.Tuple[_, _] = (leftSchema <*> rightSchema).asInstanceOf[Schema.Tuple[_, _]]

          val accessor = tupleSchema.makeAccessors(builder)

          assert(
            accessor match {
              case (Lens(r1, f1), Lens(r2, f2)) =>
                r1 == r2 && r2 == tupleSchema.toRecord &&
                  f1.label == "_1" && f1.schema == leftSchema &&
                  f2.label == "_2" && f2.schema == rightSchema
              case _ => false
            }
          )(isTrue)
      }
    },
    testM("either") {
      check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
        case (leftSchema, rightSchema) =>
          val eitherSchema: Schema.EitherSchema[_, _] =
            (rightSchema <+> leftSchema).asInstanceOf[Schema.EitherSchema[_, _]]
          val accessor = eitherSchema.makeAccessors(builder)

          assert(
            accessor match {
              case (
                  Prism(e1, Case("Right", c1, _, _)),
                  Prism(e2, Case("Left", c2, _, _))
                  ) =>
                e1 == e2 && e2 == eitherSchema.toEnum &&
                  c1 == eitherSchema.rightSchema && c2 == eitherSchema.leftSchema
              case a =>
                println(s"fallthrough $a")
                false
            }
          )(isTrue)
      }
    },
    testM("lazy") {
      check(SchemaGen.anyPrimitive) { schema =>
        val lazySchema         = Schema.defer(schema)
        val eagerAccessor: Any = schema.makeAccessors(builder)
        val lazyAccessor: Any  = lazySchema.makeAccessors(builder)

        assert(eagerAccessor == lazyAccessor)(isTrue)
      }
    },
    test("case class") {
      val schema: Schema.CaseClass3[String, SchemaGen.Arity2, SchemaGen.Arity1, SchemaGen.Arity3] =
        Schema[SchemaGen.Arity3]
          .asInstanceOf[Schema.CaseClass3[String, SchemaGen.Arity2, SchemaGen.Arity1, SchemaGen.Arity3]]
      val accessor = schema.makeAccessors(builder)

      assertTrue(
        accessor match {
          case (Lens(r1, f1), Lens(r2, f2), Lens(r3, f3)) =>
            r1 == schema && r2 == schema && r3 == schema && f1 == schema.field1 && f2 == schema.field2 && f3 == schema.field3
          case _ => false

        }
      )
    },
    test("sealed trait") {
      val (Prism(s1, c1), Prism(s2, c2), Prism(s3, c3), Prism(s4, c4), Prism(s5, c5), Prism(s6, c6)) =
        SchemaGen.Json.schema.makeAccessors(builder)

      assertTrue(
        s1 == schema && s2 == schema && s3 == schema & s4 == schema && s5 == schema && s6 == schema &&
          c1.id == "JArray" &&
          c2.id == "JDecimal" &&
          c3.id == "JNull" &&
          c4.id == "JNumber" &&
          c5.id == "JObject" &&
          c6.id == "JString"
      )
    }
  )
}
