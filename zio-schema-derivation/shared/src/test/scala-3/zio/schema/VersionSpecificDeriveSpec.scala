package zio.schema

import zio.*
import zio.test.*
import zio.test.Assertion.*
import scala.reflect.ClassTag

trait VersionSpecificDeriveSpec extends ZIOSpecDefault {
  import VersionSpecificDeriveSpec.*

  def versionSpecificSuite = 
    suite("Scala 3 specific tests")(
      test("Opaque types") {
        val deriver = new Deriver[TC] {
          override def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[Deriver.WrappedF[TC, _]], summoned: => Option[TC[A]]): TC[A] = ???

          override def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[Deriver.WrappedF[TC, _]], summoned: => Option[TC[A]]): TC[A] = ???

          override def derivePrimitive[A](st: StandardType[A], summoned: => Option[TC[A]]): TC[A] = ???

          override def derivePrimitiveAlias[A: ClassTag, U](st: StandardType[U], summoned: => Option[TC[A]]): TC[A] = {
            println(s"deriving primitive alias for ${implicitly[ClassTag[A]]} with $st"); 
            if (st == StandardType.StringType) {
              new TC[A] {
                def name(a: A): String = a.asInstanceOf[String]
              }
            } else {
              ???
            }
          }

          override def deriveOption[A](option: Schema.Optional[A], inner: => TC[A], summoned: => Option[TC[Option[A]]]): TC[Option[A]] = ???

          override def deriveSequence[C[_], A](sequence: Schema.Sequence[C[A], A, _], inner: => TC[A], summoned: => Option[TC[C[A]]]): TC[C[A]] = ???

          override def deriveMap[K, V](map: Schema.Map[K, V], key: => TC[K], value: => TC[V], summoned: => Option[TC[Map[K, V]]]): TC[Map[K, V]] = ???

          override def deriveTransformedRecord[A, B](
            record: Schema.Record[A],
            transform: Schema.Transform[A, B, _],
            fields: => Chunk[Deriver.WrappedF[TC, _]],
            summoned: => Option[TC[B]]
          ): TC[B] = ???        
        }

        given TC[AnotherObject.MyId] = deriver.derive

        def show[A](a: A)(using ev: TC[A]): String =
          ev.name(a)

        assert(show(AnotherObject.MyId("abc")))(equalTo("abc"))
      }
    )
}

object VersionSpecificDeriveSpec {
  object AnotherObject {
    opaque type MyId = String

    object MyId {
      def apply(s: String): MyId = s

      given(using ev: Schema[String]): Schema[MyId] = ev
    }
  }

  trait TC[A] {
    def name(a: A): String
  }
}
