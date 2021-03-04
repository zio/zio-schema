package zio.schema

import zio.Chunk

sealed trait Schema[A] { self =>
  def ? : Schema[Option[A]] = Schema.Optional(self)

  def transform[B](f: A => B, g: B => A): Schema[B] =
    Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)))

  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Schema[B] =
    Schema.Transform[A, B](self, f, g)

  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple(self, that)

}

object Schema {
  sealed case class Record(structure: Map[String, Schema[_]])      extends Schema[Map[String, _]]
  sealed case class Sequence[A](element: Schema[A])                extends Schema[Chunk[A]]
  sealed case class Enumeration(structure: Map[String, Schema[_]]) extends Schema[Map[String, _]]
  sealed case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Schema[B]
  sealed case class Primitive[A](standardType: StandardType[A])    extends Schema[A]
  sealed case class Tuple[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]
  sealed case class Optional[A](codec: Schema[A])                  extends Schema[Option[A]]
  final case class Fail[A](message: String)                        extends Schema[A]

  def fail[A](message: String): Schema[A] = Fail(message)

  def apply[A](implicit codec: Schema[A]): Schema[A] = codec

  def caseClassN[A, Z](t1: (String, Schema[A]))(f: A => Z, g: Z => Option[A]): Schema[Z] =
    Schema
      .record(Map(t1))
      .transformOrFail(
        { map =>
          val v1 = map(t1._1).asInstanceOf[A]

          Right(f(v1))
        }, { (z: Z) =>
          g(z).map { a =>
            Map(t1._1 -> a)
          }.toRight("Cannot deconstruct case class")
        }
      )

  def caseClassN[A, B, Z](
    t1: (String, Schema[A]),
    t2: (String, Schema[B])
  )(f: (A, B) => Z, g: Z => Option[(A, B)]): Schema[Z] =
    Schema
      .record(Map[String, Schema[_]](t1, t2))
      .transformOrFail(
        { map =>
          val v1 = map(t1._1).asInstanceOf[A]
          val v2 = map(t2._1).asInstanceOf[B]

          Right(f(v1, v2))
        }, { (z: Z) =>
          g(z).map { case (a, b) => Map(t1._1 -> a, t2._1 -> b) }
            .toRight("Cannot deconstruct case class")
        }
      )

  def caseClassN[A, B, C, Z](
    t1: (String, Schema[A]),
    t2: (String, Schema[B]),
    t3: (String, Schema[C])
  )(f: (A, B, C) => Z, g: Z => Option[(A, B, C)]): Schema[Z] =
    Schema
      .record(Map[String, Schema[_]](t1, t2, t3))
      .transformOrFail(
        { map =>
          val v1 = map(t1._1).asInstanceOf[A]
          val v2 = map(t2._1).asInstanceOf[B]
          val v3 = map(t3._1).asInstanceOf[C]

          Right(f(v1, v2, v3))
        }, { (z: Z) =>
          g(z).map { case (a, b, c) => Map(t1._1 -> a, t2._1 -> b, t3._1 -> c) }
            .toRight("Cannot deconstruct case class")
        }
      )

  def either[A, B](left: Schema[A], right: Schema[B]): Schema[Either[A, B]] =
    enumeration(Map("Left" -> left, "Right" -> right)).transformOrFail(
      { map =>
        map.headOption.map {
          case ("Left", v)  => Right(Left(v.asInstanceOf[A]))
          case ("Right", v) => Right(Right(v.asInstanceOf[B]))
          case _            => Left("Expected left or right of sum")
        }.getOrElse(Left("Expected left or right of sum"))
      }, {
        case Left(v)  => Right(Map("Left"  -> v))
        case Right(v) => Right(Map("Right" -> v))
      }
    )

  def enumeration(structure: Map[String, Schema[_]]): Schema[Map[String, _]] =
    Enumeration(structure)

  def first[A](codec: Schema[(A, Unit)]): Schema[A] =
    codec.transform[A](_._1, a => (a, ()))

  implicit def list[A](implicit element: Schema[A]): Schema[List[A]] =
    sequence(element).transform(_.toList, Chunk.fromIterable(_))

  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType)

  def record(structure: Map[String, Schema[_]]): Schema[Map[String, _]] =
    Record(structure)

  implicit def sequence[A](implicit element: Schema[A]): Schema[Chunk[A]] =
    Sequence(element)

  implicit def set[A](implicit element: Schema[A]): Schema[Set[A]] =
    sequence(element).transform(_.toSet, Chunk.fromIterable(_))

  def second[A](codec: Schema[(Unit, A)]): Schema[A] =
    codec.transform[A](_._2, a => ((), a))

  implicit def vector[A](implicit element: Schema[A]): Schema[Vector[A]] =
    sequence(element).transform(_.toVector, Chunk.fromIterable(_))

  implicit def zipN[A, B](implicit c1: Schema[A], c2: Schema[B]): Schema[(A, B)] =
    c1.zip(c2)

  implicit def zipN[A, B, C](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C]): Schema[(A, B, C)] =
    c1.zip(c2).zip(c3).transform({ case ((a, b), c) => (a, b, c) }, { case (a, b, c) => ((a, b), c) })

  implicit def zipN[A, B, C, D](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D]
  ): Schema[(A, B, C, D)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .transform({ case (((a, b), c), d) => (a, b, c, d) }, { case (a, b, c, d) => (((a, b), c), d) })
}
