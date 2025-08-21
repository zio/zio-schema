package zio.schema
package meta

import zio.schema.meta.AstRenderer.render
import zio.schema._
import zio.schema.meta.MetaRenderSpec.HashAlgo.Blake3
import zio.test._

object MetaRenderSpec extends ZIOSpecDefault {

  final case class Sample(id: Int, name: String)

  object Sample {
    implicit val schema: Schema[Sample] =
      Schema.CaseClass2(
        TypeId.parse("zio.schema.meta.Sample"),
        field01 = Schema.Field[Sample, Int]("id", Schema.primitive[Int], get0 = _.id, set0 = (s, v) => s.copy(id = v)),
        field02 = Schema.Field[Sample, String]("name", Schema.primitive[String], get0 = _.name, set0 = (s, v) => s.copy(name = v)),
        Sample.apply
      )
  }


  final case class Sample2(one: Int, two: (String, Int), three: ((String, Int), Map[String, List[String]]))
  object Sample2 {
    implicit val schema: Schema[Sample2] =
      Schema.CaseClass3(
        TypeId.parse("zio.schema.meta.Sample2"),
        field01 = Schema.Field[Sample2, Int]("one", Schema.primitive[Int], get0 = _.one, set0 = (h, o) => h.copy(one = o)),
        field02 = Schema.Field[Sample2, (String, Int)]("two", Schema.tuple2(Schema.primitive[String], Schema.primitive[Int]), get0 = _.two, set0 = (h, o) => h.copy(two = o)),
        field03 = Schema.Field[Sample2, ((String, Int), Map[String, List[String]])](
          "three",
          Schema.tuple2(
            Schema.tuple2(Schema.primitive[String], Schema.primitive[Int]),
            Schema.map(Schema.primitive[String], Schema.list(Schema.primitive[String]))
          ),
          get0 = _.three, set0 = (h, o) => h.copy(three = o)
        ),
        Sample2.apply
      )
  }




  sealed trait HashAlgo

  object HashAlgo {

    implicit val schema: Schema[HashAlgo] = DeriveSchema.gen

    case object Blake3 extends HashAlgo
    case object SHA256 extends HashAlgo
    case object SHA512 extends HashAlgo
    case class Custom(name: String) extends HashAlgo
  }



  override def spec: Spec[Any, Nothing] = suite("AstRendererSpec")(
    test("renders simple case class schema correctly") {
      val meta = ExtensibleMetaSchema.fromSchema(Schema[Sample])
      val rendered = AstRenderer.render(meta)

      val expectedLines = List(
        "record",
        "├── id: int",
        "└── name: string"
      )

      assertTrue(expectedLines.forall(s => rendered.contains(s)))
    },
    test("renders deeply nested case class with tuples and map/list") {
      val schema = Schema[Sample2]
      val meta = ExtensibleMetaSchema.fromSchema(schema)
      val rendered = render(meta)
      val expected =
        """record
          |├── one: int
          |├── two: tuple
          |│   ├── left: string
          |│   └── right: int
          |└── three: tuple
          |    ├── left: tuple
          |    │   ├── left: string
          |    │   └── right: int
          |    └── right: map
          |        ├── keys: string
          |        └── values: list
          |            └── item: string""".stripMargin

      assertTrue(rendered == expected)
    },
    test("renders enums with case objects only") {
      val schema = Schema[HashAlgo]
      schema.ast
      val meta = schema.ast

      println(zio.schema.codec.BinaryCodec)

      println(schema.toDynamic(Blake3) match {
        case DynamicValue.Enumeration(id, value) => value._2 match {
          case DynamicValue.Record(id, value) => value
        }
      })
      println(Schema[Sample].toDynamic(Sample(0, "name")))


      val rendered = render(meta)

      println(rendered)
      val expected =
        """enum:
          |├── Blake3
          |├── SHA256
          |├── SHA512
          |└── Custom: record
          |    └── name: string""".stripMargin
      assertTrue(rendered == expected)
    },
    test("tests serializable call") {
      val schema = Schema[HashAlgo]
      val meta = schema.ast.toSchema
      val serializable = schema.serializable
      SchemaAssertions.hasSameSchemaStructure(serializable).run(meta)
    }
  )
}
