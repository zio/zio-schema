package zio.schema

import zio.schema.Schema._
import zio.test._

object AccessorBuilderSpec extends DefaultRunnableSpec {
  import TestAccessorBuilder._

  private val builder: TestAccessorBuilder = new TestAccessorBuilder

  override def spec: ZSpec[Environment, Failure] = suite("AccessorBuilder")(
    test("fail") {
      val accessor: Any = Schema.fail("error").makeAccessors(builder)
      assertTrue(accessor == ())
    },
    testM("primitive") {
      check(SchemaGen.anyPrimitive) { schema =>
        assertTrue(schema.makeAccessors(builder) == ())
      }
    },
    testM("sequence") {
      check(SchemaGen.anySchema) { elementSchema =>
        val collectionSchema = elementSchema.repeated
        val traversal        = collectionSchema.makeAccessors(builder)

        assertTrue(
          traversal match {
            case Traversal(col, elem) =>
              col == collectionSchema && elem == elementSchema
            case _ => false
          }
        )
      }
    },
    testM("transform") {
      check(SchemaGen.anyPrimitive) { schema =>
        val transform = schema.transformOrFail[Unit](_ => Left("error"), _ => Left("error"))

        val transformAccessor: Any = transform.makeAccessors(builder).asInstanceOf[Any]
        val schemaAccessor: Any    = schema.makeAccessors(builder).asInstanceOf[Any]

        assertTrue(
          transformAccessor == schemaAccessor
        )
      }
    },
    testM("optional") {
      check(SchemaGen.anyPrimitive) { schema =>
        val optionalSchema: Schema.Optional[_] = schema.optional.asInstanceOf[Schema.Optional[_]]
        val enumSchema                         = optionalSchema.toEnum
        val accessor                           = optionalSchema.makeAccessors(builder)

        assertTrue(
          accessor match {
            case (
                Prism(e1, Case("Some", c1, _)),
                Prism(e2, Case("None", _, _))
                ) =>
              e1 == e2 && e2 == enumSchema && c1 == optionalSchema.someCodec
            case _ => false
          }
        )
      }
    },
    testM("tuple") {
      check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
        case (leftSchema, rightSchema) =>
          val tupleSchema: Schema.Tuple[_, _] = (leftSchema <*> rightSchema).asInstanceOf[Schema.Tuple[_, _]]

          val accessor = tupleSchema.makeAccessors(builder)

          assertTrue(
            accessor match {
              case (Lens(r1, f1), Lens(r2, f2)) =>
                r1 == r2 && r2 == tupleSchema.toRecord &&
                  f1.label == "_1" && f1.schema == leftSchema &&
                  f2.label == "_2" && f2.schema == rightSchema
              case _ => false
            }
          )
      }
    },
    testM("either") {
      check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
        case (leftSchema, rightSchema) =>
          val eitherSchema: Schema.EitherSchema[_, _] =
            (rightSchema <+> leftSchema).asInstanceOf[Schema.EitherSchema[_, _]]
          val accessor = eitherSchema.makeAccessors(builder)

          assertTrue(
            accessor match {
              case (
                  Prism(e1, Case("Right", c1, _)),
                  Prism(e2, Case("Left", c2, _))
                  ) =>
                e1 == e2 && e2 == eitherSchema.toEnum &&
                  c1 == eitherSchema.rightSchema && c2 == eitherSchema.leftSchema
              case a =>
                println(s"fallthrough $a")
                false
            }
          )
      }
    },
    testM("lazy") {
      check(SchemaGen.anyPrimitive) { schema =>
        val lazySchema         = Schema.defer(schema)
        val eagerAccessor: Any = schema.makeAccessors(builder)
        val lazyAccessor: Any  = lazySchema.makeAccessors(builder)

        assertTrue(eagerAccessor == lazyAccessor)
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
      val schema: Schema.EnumN[_, _] = Schema[SchemaGen.Json].asInstanceOf[Schema.EnumN[_, _]]
      val cases                      = schema.structure
      val accessor                   = schema.makeAccessors(builder)

      assertTrue(
        accessor match {
          case (
              Prism(s1, c1),
              (Prism(s2, c2), (Prism(s3, c3), (Prism(s4, c4), (Prism(s5, c5), (Prism(s6, c6), ())))))
              ) =>
            s1 == schema && s2 == schema && s3 == schema & s4 == schema && s5 == schema && s6 == schema &&
              c1.codec == cases("JArray") &&
              c2.codec == cases("JDecimal") &&
              c3.codec == cases("JNull") &&
              c4.codec == cases("JNumber") &&
              c5.codec == cases("JObject") &&
              c6.codec == cases("JString")
          case _ => false
        }
      )
    }
  )
}
