package zio.schema

import scala.collection.immutable.ListMap

import zio._
import zio.constraintless.TypeList._
import zio.schema.CaseSet._
import zio.schema.SchemaAssertions._
import zio.schema.meta.ExtensibleMetaSchema.Labelled
import zio.schema.meta.{ ExtensibleMetaSchema, MetaSchema, NodePath }
import zio.test._

object MetaSchemaSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] = suite("MetaSchema")(
    suite("from schema")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(MetaSchema.fromSchema(s) == ExtensibleMetaSchema.Value[DynamicValue :: End](typ))
        }
      }
    ),
    suite("optional")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(
              MetaSchema.fromSchema(s.optional) == ExtensibleMetaSchema.Value[DynamicValue :: End](typ, optional = true)
            )
        }
      }
    ),
    suite("sequence")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(
              MetaSchema.fromSchema(Schema.chunk(s)) == ExtensibleMetaSchema
                .ListNode(
                  ExtensibleMetaSchema.Value[DynamicValue :: End](typ, path = NodePath.root / "item"),
                  NodePath.root
                )
            )
        }
      }
    ),
    suite("record")(
      test("generic") {
        val schema =
          Schema.record(
            TypeId.parse("zio.schema.MetaSchema.Product"),
            FieldSet(
              Seq(
                Schema.Field[ListMap[String, _], String](
                  "a",
                  Schema[String],
                  get0 = _("a").asInstanceOf[String],
                  set0 = (r, v) => r.updated("a", v)
                ),
                Schema.Field[ListMap[String, _], Int](
                  "b",
                  Schema[Int],
                  get0 = _("b").asInstanceOf[Int],
                  set0 = (r, v) => r.updated("b", v)
                )
              ): _*
            )
          )
        val expectedAst =
          ExtensibleMetaSchema.Product(
            id = TypeId.parse("zio.schema.MetaSchema.Product"),
            path = NodePath.root,
            fields = Chunk(
              Labelled(
                "a",
                ExtensibleMetaSchema.Value[DynamicValue :: End](StandardType.StringType, NodePath.root / "a")
              ),
              Labelled("b", ExtensibleMetaSchema.Value[DynamicValue :: End](StandardType.IntType, NodePath.root / "b"))
            )
          )
        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      },
      test("case class") {
        val schema = Schema[SchemaGen.Arity2]
        val expectedAst =
          ExtensibleMetaSchema.Product(
            id = TypeId.parse("zio.schema.SchemaGen.Arity2"),
            path = NodePath.root,
            fields = Chunk(
              Labelled(
                "value1",
                ExtensibleMetaSchema
                  .Value[DynamicValue :: End](StandardType.StringType, NodePath.root / "value1")
              ),
              Labelled(
                "value2",
                ExtensibleMetaSchema.Product(
                  id = TypeId.parse("zio.schema.SchemaGen.Arity1"),
                  path = NodePath.root / "value2",
                  fields = Chunk(
                    Labelled(
                      "value",
                      ExtensibleMetaSchema
                        .Value[DynamicValue :: End](StandardType.IntType, NodePath.root / "value2" / "value")
                    )
                  )
                )
              )
            )
          )

        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      },
      test("recursive case class") {
        val schema = Schema[Recursive]
        val ast    = MetaSchema.fromSchema(schema)

        val recursiveRef: Option[MetaSchema] = ast match {
          case ExtensibleMetaSchema.Product(_, _, elements, _) =>
            elements.find {
              case Labelled("r", _) => true
              case _                => false
            }.map(_.schema)
          case _ => None
        }
        assertTrue(
          recursiveRef.exists {
            case ExtensibleMetaSchema.Ref(pathRef, _, _) => pathRef == Chunk.empty
            case _                                       => false
          }
        )
      }
    ),
    suite("enumeration")(
      test("generic") {
        val schema =
          Schema.enumeration[Any, CaseSet.Aux[Any]](
            TypeId.Structural,
            caseOf[SchemaGen.Arity1, Any]("type1")(_.asInstanceOf[SchemaGen.Arity1])(_.asInstanceOf[Any])(
              _.isInstanceOf[Any]
            ) ++ caseOf[SchemaGen.Arity2, Any](
              "type2"
            )(_.asInstanceOf[SchemaGen.Arity2])(_.asInstanceOf[Any])(_.isInstanceOf[Any])
          )
        val expectedAst =
          ExtensibleMetaSchema.Sum(
            TypeId.Structural,
            path = NodePath.root,
            cases = Chunk(
              Labelled(
                "type1",
                ExtensibleMetaSchema.Product(
                  id = TypeId.parse("zio.schema.SchemaGen.Arity1"),
                  path = NodePath.root / "type1",
                  fields = Chunk(
                    Labelled(
                      "value",
                      ExtensibleMetaSchema
                        .Value[DynamicValue :: End](StandardType.IntType, NodePath.root / "type1" / "value")
                    )
                  )
                )
              ),
              Labelled(
                "type2",
                ExtensibleMetaSchema.Product(
                  id = TypeId.parse("zio.schema.SchemaGen.Arity2"),
                  path = NodePath.root / "type2",
                  fields = Chunk(
                    Labelled(
                      "value1",
                      ExtensibleMetaSchema
                        .Value[DynamicValue :: End](StandardType.StringType, NodePath.root / "type2" / "value1")
                    ),
                    Labelled(
                      "value2",
                      ExtensibleMetaSchema.Product(
                        id = TypeId.parse("zio.schema.SchemaGen.Arity1"),
                        path = NodePath.root / "type2" / "value2",
                        fields = Chunk(
                          Labelled(
                            "value",
                            ExtensibleMetaSchema
                              .Value[DynamicValue :: End](
                                StandardType.IntType,
                                NodePath.root / "type2" / "value2" / "value"
                              )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      },
      test("sealed trait") {
        val schema = Schema[Pet]
        val expectedAst = ExtensibleMetaSchema.Sum[DynamicValue :: End](
          TypeId.parse("zio.schema.MetaSchemaSpec.Pet"),
          path = NodePath.root,
          cases = Chunk(
            Labelled(
              "Rock",
              ExtensibleMetaSchema.Product[DynamicValue :: End](
                id = TypeId.parse("zio.schema.MetaSchemaSpec.Rock"),
                path = NodePath.root / "Rock",
                fields = Chunk.empty
              )
            ),
            Labelled(
              "Dog",
              ExtensibleMetaSchema.Product[DynamicValue :: End](
                id = TypeId.parse("zio.schema.MetaSchemaSpec.Dog"),
                path = NodePath.root / "Dog",
                fields = Chunk(
                  Labelled(
                    "name",
                    ExtensibleMetaSchema
                      .Value[DynamicValue :: End](StandardType.StringType, NodePath.root / "Dog" / "name")
                  )
                )
              )
            ),
            Labelled(
              "Cat",
              ExtensibleMetaSchema.Product[DynamicValue :: End](
                id = TypeId.parse("zio.schema.MetaSchemaSpec.Cat"),
                path = NodePath.root / "Cat",
                fields = Chunk(
                  Labelled(
                    "name",
                    ExtensibleMetaSchema
                      .Value[DynamicValue :: End](StandardType.StringType, NodePath.root / "Cat" / "name")
                  ),
                  Labelled(
                    "hasHair",
                    ExtensibleMetaSchema
                      .Value[DynamicValue :: End](StandardType.BoolType, NodePath.root / "Cat" / "hasHair")
                  )
                )
              )
            )
          )
        )
        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      }
    ),
    suite("materialization")(
      test("simple recursive product") {
        val schema = Recursive.schema
        assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema.asInstanceOf[Schema[_]]))
      },
      test("simple recursive sum") {
        val schema = RecursiveSum.schema
        assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema.asInstanceOf[Schema[_]]))
      },
      test("primitive") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema.asInstanceOf[Schema[_]]))
        }
      },
      test("optional") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(MetaSchema.fromSchema(schema.optional).toSchema)(hasSameSchemaStructure(schema.optional))
        }
      },
      test("sequence") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(MetaSchema.fromSchema(schema.repeated).toSchema)(hasSameSchemaStructure(schema.repeated))
        }
      },
      test("tuple") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(MetaSchema.fromSchema(left <*> right).toSchema)(hasSameSchemaStructure(left <*> right))
        }
      },
      test("either") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(MetaSchema.fromSchema(left <+> right).toSchema)(hasSameSchemaStructure(left <+> right))
        }
      },
      test("case class") {
        check(SchemaGen.anyCaseClassSchema) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      } @@ TestAspect.ignore, //annotations are missing in the meta schema
      test("sealed trait") {
        check(SchemaGen.anyEnumSchema) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      } @@ TestAspect.ignore, //annotations are missing in the meta schema
      test("recursive type") {
        check(SchemaGen.anyRecursiveType) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      },
      test("any schema") {
        check(SchemaGen.anySchema) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      } @@ TestAspect.shrinks(0),
      test("sequence of optional primitives") {
        val schema       = Schema[List[Option[Int]]]
        val materialized = MetaSchema.fromSchema(schema).toSchema
        assert(materialized)(hasSameSchemaStructure(schema))
      },
      test("optional sequence of primitives") {
        val schema       = Schema[Option[List[String]]]
        val materialized = MetaSchema.fromSchema(schema).toSchema
        assert(materialized)(hasSameSchemaStructure(schema))
      },
      test("dynamic") {
        check(SchemaGen.anyDynamic) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      }
    ),
    suite("extended meta schema")(
      test("represents known type as Known") {
        val meta1 = ExtendedMetaSchema.fromSchema(Schema[Pet])
        val meta2 = ExtendedMetaSchema.fromSchema(Schema[DynamicValue])
        val meta3 = ExtendedMetaSchema.fromSchema(Schema[Recursive])

        assertTrue(
          meta1.isInstanceOf[ExtensibleMetaSchema.Known[_]],
          meta2.isInstanceOf[ExtensibleMetaSchema.Known[_]],
          meta3.isInstanceOf[ExtensibleMetaSchema.Product[_]]
        )
      },
      test("materializes the original schema") {
        val meta1 = ExtendedMetaSchema.fromSchema(Schema[Pet])
        val meta2 = ExtendedMetaSchema.fromSchema(Schema[DynamicValue])

        val refEq1 = meta1.toSchema eq Schema[Pet]
        val refEq2 = meta2.toSchema eq Schema[DynamicValue]
        assertTrue(refEq1, refEq2)
      },
      test("roundtrip serialization with known types") {
        val meta1 = ExtendedMetaSchema.fromSchema(Schema[Pet])
        val meta2 = ExtendedMetaSchema.fromSchema(Schema[DynamicValue])
        val meta3 = ExtendedMetaSchema.fromSchema(Schema[Recursive])

        val dyn1 = DynamicValue(meta1)
        val dyn2 = DynamicValue(meta2)
        val dyn3 = DynamicValue(meta3)

        val meta12 = dyn1.toTypedValue(ExtendedMetaSchema.schema).toOption.get
        val meta22 = dyn2.toTypedValue(ExtendedMetaSchema.schema).toOption.get
        val meta32 = dyn3.toTypedValue(ExtendedMetaSchema.schema).toOption.get

        val refEq1 = meta12.toSchema eq Schema[Pet]
        val refEq2 = meta22.toSchema eq Schema[DynamicValue]

        assertTrue(
          refEq1,
          refEq2,
          meta32.isInstanceOf[ExtensibleMetaSchema.Product[_]]
        )
      }
    )
  )

  case class Recursive(id: String, r: Option[Recursive])

  object Recursive {
    implicit lazy val schema: Schema[Recursive] = DeriveSchema.gen[Recursive]
  }

  sealed trait RecursiveSum

  object RecursiveSum {
    case object Leaf                                                              extends RecursiveSum
    final case class Node(label: String, left: RecursiveSum, right: RecursiveSum) extends RecursiveSum

    implicit lazy val schema: Schema[RecursiveSum] = DeriveSchema.gen[RecursiveSum]
  }

  sealed trait Pet
  case object Rock             extends Pet
  case class Dog(name: String) extends Pet

  object Dog {
    implicit lazy val schema: Schema[Dog] = DeriveSchema.gen[Dog]
  }
  case class Cat(name: String, hasHair: Boolean) extends Pet

  object Cat {
    implicit lazy val schema: Schema[Cat] = DeriveSchema.gen[Cat]
  }

  object Pet {
    implicit lazy val schema: Schema[Pet] = DeriveSchema.gen[Pet]
  }

  type ExtendedMetaSchema = ExtensibleMetaSchema[DynamicValue :: Pet :: End]

  object ExtendedMetaSchema {
    lazy val schema: Schema[ExtendedMetaSchema] =
      ExtensibleMetaSchema.schema[DynamicValue :: Pet :: End]

    def fromSchema[A](schema: Schema[A]): ExtendedMetaSchema =
      ExtensibleMetaSchema.fromSchema(schema)
  }
}
