package zio.schema.codec

import zio.Scope
import zio.bson.BsonBuilder._
import zio.bson._
import zio.schema.codec.BsonSchemaCodecSpec.roundTripTest
import zio.schema.{ DeriveGen, DeriveSchema, Schema }
import zio.test._

/**
 * Not supported in scala 3 yet
 */
object BsonSchemaCodecGenericSpec extends ZIOSpecDefault {
  case class SimpleGeneric[T](value: T)

  object SimpleGeneric {
    implicit def schema[T: Schema]: Schema[SimpleGeneric[T]]   = DeriveSchema.gen
    implicit def codec[T: Schema]: BsonCodec[SimpleGeneric[T]] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait GenericTree[T]

  object GenericTree {
    case class Branch[T](left: GenericTree[T], right: GenericTree[T]) extends GenericTree[T]

    object Branch {
      implicit def schema[T: Schema]: Schema[Branch[T]] = DeriveSchema.gen[Branch[T]]
    }

    case class Leaf[T](value: T) extends GenericTree[T]

    object Leaf {
      implicit def schema[T: Schema]: Schema[Leaf[T]] = DeriveSchema.gen
    }

    implicit def schema[T: Schema]: Schema[GenericTree[T]]   = DeriveSchema.gen[GenericTree[T]]
    implicit def codec[T: Schema]: BsonCodec[GenericTree[T]] = BsonSchemaCodec.bsonCodec(schema)

    private def genLeafOf[R, A](gen: Gen[R, A]) = gen.map(Leaf(_))

    def genOf[R, A](gen: Gen[R, A]): Gen[R, GenericTree[A]] = Gen.sized { i =>
      if (i >= 2) Gen.oneOf(genLeafOf(gen), Gen.suspend(genOf(gen).zipWith(genOf(gen))(Branch(_, _))).resize(i / 2))
      else genLeafOf(gen)
    }
  }

  case class GenericRec[T](t: T, next: Option[GenericRec[T]])

  object GenericRec {
    implicit def schema[T: Schema]: Schema[GenericRec[T]]   = DeriveSchema.gen
    implicit def codec[T: Schema]: BsonCodec[GenericRec[T]] = BsonSchemaCodec.bsonCodec(schema)
  }

  def spec: Spec[TestEnvironment with Scope, Any] = suite("BsonSchemaCodecSpec")(
    suite("round trip")(
      roundTripTest("SimpleGeneric[String]")(
        DeriveGen.gen[SimpleGeneric[String]],
        SimpleGeneric("str"),
        doc("value" -> str("str"))
      ),
      roundTripTest("GenericRec[Int]")(
        DeriveGen.gen[GenericRec[Int]],
        GenericRec(1, Some(GenericRec(2, None))),
        doc("t" -> int(1), "next" -> doc("t" -> int(2)))
      ),
      roundTripTest("GenericTree[Int]")(
        GenericTree.genOf(Gen.int),
        GenericTree.Leaf(1),
        doc("Leaf" -> doc("value" -> int(1)))
      )
    )
  )

}
