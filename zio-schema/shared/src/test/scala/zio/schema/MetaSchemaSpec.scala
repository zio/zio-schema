// package zio.schema

// import java.time.format.DateTimeFormatter

// import zio.test.Assertion._
// import zio.test.environment.{ Live, TestClock, TestConsole, TestRandom, TestSystem }
// import zio.test.{ Annotations, Sized, TestConfig, ZSpec, _ }
// import zio.{ ZEnv, _ }

// object MetaSchemaSpec extends DefaultRunnableSpec {
//   import SchemaAssertions._

//   def spec: ZSpec[
//     Annotations with Live with Sized with TestClock with TestConfig with TestConsole with TestRandom with TestSystem with ZEnv,
//     Any
//   ] = suite("MetaSchema")(
//     suite("schema to meta")(
//       testM("standard types") {
//         check(StandardTypeGen.anyStandardType) {
//           case tpe @ StandardType.Duration(units) =>
//             assertSerializesTo(Schema.Primitive(tpe), MetaSchema.Duration(units))
//           case tpe =>
//             assertSerializesTo(Schema.Primitive(tpe), MetaSchema.Value(tpe))

//         }
//       },
//       testM("optional") {
//         check(SchemaGen.anySchema) { schema =>
//           assertSerializesTo(schema.optional, MetaSchema.Optional(MetaSchema.fromSchema(schema)))
//         }
//       },
//       testM("fail") {
//         check(Gen.anyString) { message =>
//           assertSerializesTo(Schema.fail(message), MetaSchema.Fail(message))
//         }
//       },
//       testM("tuple") {
//         check(SchemaGen.anySchema <*> SchemaGen.anySchema) {
//           case (leftSchema, rightSchema) =>
//             assertSerializesTo(
//               leftSchema <*> rightSchema,
//               MetaSchema.TupleMeta(MetaSchema.fromSchema(leftSchema), MetaSchema.fromSchema(rightSchema))
//             )
//         }
//       },
//       testM("either") {
//         check(SchemaGen.anySchema <*> SchemaGen.anySchema) {
//           case (leftSchema, rightSchema) =>
//             assertSerializesTo(
//               leftSchema <+> rightSchema,
//               MetaSchema.EitherMeta(MetaSchema.fromSchema(leftSchema), MetaSchema.fromSchema(rightSchema))
//             )
//         }
//       },
//       testM("sequence") {
//         check(SchemaGen.anyPrimitive) { schema =>
//           assertSerializesTo(Schema.list(schema), MetaSchema.Sequence(MetaSchema.fromSchema(schema)))
//         }
//       },
//       testM("record") {
//         check(SchemaGen.anyRecord) { schema =>
//           val expectedStructure: Chunk[MetaSchema.MetaField] =
//             schema.asInstanceOf[Schema.GenericRecord].structure.map { field =>
//               MetaSchema.MetaField(field.label, MetaSchema.fromSchema(field.schema))
//             }
//           assertSerializesTo(schema, MetaSchema.Record(expectedStructure))
//         }
//       }
//     ),
//     suite("schema from meta")(
//       testM("standard types") {
//         check(StandardTypeGen.anyStandardType) {
//           case tpe @ StandardType.Duration(units) =>
//             assertDeserializesTo(MetaSchema.Duration(units), Schema.Primitive(tpe))
//           case tpe @ StandardType.Instant(_) =>
//             assertDeserializesTo(
//               MetaSchema.Value(tpe),
//               Schema.Primitive(StandardType.Instant(DateTimeFormatter.ISO_INSTANT))
//             )
//           case tpe @ StandardType.LocalTime(_) =>
//             assertDeserializesTo(
//               MetaSchema.Value(tpe),
//               Schema.Primitive(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME))
//             )
//           case tpe @ StandardType.LocalDateTime(_) =>
//             assertDeserializesTo(
//               MetaSchema.Value(tpe),
//               Schema.Primitive(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
//             )
//           case tpe @ StandardType.LocalDate(_) =>
//             assertDeserializesTo(
//               MetaSchema.Value(tpe),
//               Schema.Primitive(StandardType.LocalDate(DateTimeFormatter.ISO_LOCAL_DATE))
//             )
//           case tpe @ StandardType.ZonedDateTime(_) =>
//             assertDeserializesTo(
//               MetaSchema.Value(tpe),
//               Schema.Primitive(StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME))
//             )
//           case tpe @ StandardType.OffsetTime(_) =>
//             assertDeserializesTo(
//               MetaSchema.Value(tpe),
//               Schema.Primitive(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME))
//             )
//           case tpe @ StandardType.OffsetDateTime(_) =>
//             assertDeserializesTo(
//               MetaSchema.Value(tpe),
//               Schema.Primitive(StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
//             )
//           case tpe =>
//             assertDeserializesTo(MetaSchema.Value(tpe), Schema.Primitive(tpe))
//         }
//       },
//       testM("optional") {
//         check(SchemaGen.anyPrimitive) { schema =>
//           assertDeserializesTo(MetaSchema.Optional(MetaSchema.fromSchema(schema)), schema.optional)
//         }
//       },
//       testM("tuple") {
//         check(SchemaGen.anyTuple) { schema =>
//           assertDeserializesTo(
//             MetaSchema.TupleMeta(MetaSchema.fromSchema(schema.left), MetaSchema.fromSchema(schema.right)),
//             schema
//           )
//         }
//       },
//       testM("either") {
//         check(SchemaGen.anyEither) { schema =>
//           assertDeserializesTo(
//             MetaSchema.EitherMeta(MetaSchema.fromSchema(schema.left), MetaSchema.fromSchema(schema.right)),
//             schema
//           )
//         }
//       },
//       suite("sequence")(
//         testM("chunk") {
//           check(SchemaGen.anySchema) { schema =>
//             assertDeserializesTo(
//               MetaSchema.Sequence(MetaSchema.fromSchema(schema)),
//               Schema.chunk(schema)
//             )
//           }
//         },
//         testM("list") {
//           check(SchemaGen.anySchema) { schema =>
//             assertDeserializesTo(
//               MetaSchema.Sequence(MetaSchema.fromSchema(schema)),
//               Schema.list(schema)
//             )
//           }
//         },
//         testM("vector") {
//           check(SchemaGen.anySchema) { schema =>
//             assertDeserializesTo(
//               MetaSchema.Sequence(MetaSchema.fromSchema(schema)),
//               Schema.vector(schema)
//             )
//           }
//         }
//       )
//     )
//   )

//   def assertSerializesTo[A](schema: Schema[A], expectedMeta: MetaSchema): TestResult = {
//     val actualMeta = MetaSchema.fromSchema(schema)
//     assert(actualMeta)(equalTo(expectedMeta))
//   }

//   def assertDeserializesTo(spec: MetaSchema, expectedSchema: Schema[_]): TestResult =
//     assert(spec.toSchema)(hasSameMetaSchema(expectedSchema))

// }
