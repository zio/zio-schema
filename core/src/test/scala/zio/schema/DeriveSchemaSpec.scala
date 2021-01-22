package zio.schema

import zio.Chunk
import zio.test.{ DefaultRunnableSpec, assert, suite, test }
import zio.schema.SchemaAssertions.hasSameSchema

object DeriveSchemaSpec extends DefaultRunnableSpec {

  sealed case class UserId(id: String)

  sealed trait Status
  case class Ok(response: List[String]) extends Status
  case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
      extends Status
  case object Pending extends Status

  override def spec = suite("DeriveSchemaSpec")(
    test("DeriveSchema correctly derives schema for UserId case class") {

      val userIdSchema: Schema[UserId] = DeriveSchema.gen

      val expectedSchema: Schema[UserId] = Schema.Transform[Map[String, Any], UserId](
        Schema.Record(Map("id" -> Schema.Primitive(StandardType.StringType))),
        map => Right(UserId(map("id").asInstanceOf[String])), // ignored in equality comparison
        (userId: UserId) => Right(Map("id" -> userId.id)) // ignored in equality comparison
      )

      assert(userIdSchema)(hasSameSchema(expectedSchema))
    },
    test("DeriveSchema correctly derives schema for complex ADT") {

      val statusSchema: Schema[Status] = DeriveSchema.gen[Status]

      val expectedSchema: Schema[Status] =
        Schema.Transform[Map[String, Any], Status](
          Schema.Enumeration(
            Map(
              "Ok" -> Schema.Transform[Map[String, Any], Ok](
                Schema.Record(
                  Map(
                    "response" -> Schema.Transform[Chunk[String], List[String]](
                      Schema.Sequence(Schema.Primitive(StandardType.StringType)),
                      chunk => Right(chunk.toList),                   // ignored in equality comparison
                      elements => Right(Chunk.fromIterable(elements)) // ignored in equality comparison
                    )
                  )
                ),
                map => Right(Ok(response = List(map.toString))), // ignored in equality comparison
                ok => Right(Map("response" -> ok.response)) // ignored in equality comparison
              ),
              "Failed" -> Schema.Transform[Map[String, Any], Failed](
                Schema.Record(
                  Map(
                    "code"                  -> Schema.Primitive(StandardType.IntType),
                    "reason"                -> Schema.Primitive(StandardType.StringType),
                    "additionalExplanation" -> Schema.option(Schema.Primitive(StandardType.StringType)),
                    "remark"                -> Schema.Primitive(StandardType.StringType)
                  )
                ),
                _ => Right(Failed(1, "a", None)), // ignored in equality comparison
                failed => Right(Map("code" -> failed.code, "reason" -> failed.reason)) // ignored in equality comparison
              ),
              "Pending" -> Schema.Transform[Map[String, Any], Pending.type](
                Schema.Record(Map()),
                _ => Right(Pending), // ignored in equality comparison
                _ => Right(Map())    // ignored in equality comparison
              )
            )
          ),
          _ => Right(Pending), // ignored in equality comparison
          _ => Right(Map("Pending" -> Pending)) // ignored in equality comparison
        )

      assert(statusSchema)(hasSameSchema(expectedSchema))
    }
  )
}
