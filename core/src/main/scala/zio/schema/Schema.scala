package zio.schema

import zio.Chunk

sealed trait Schema[+A] { self =>
  def ? : Schema[Option[A]] = Schema.Optional(self)

  def transform[A2 >: A, B](f: A2 => B, g: B => A2): Schema.Transform[A2, B] =
    Schema.Transform[A2, B](self, a => Right(f(a)), b => Right(g(b)))

  def transformOrFail[A2 >: A, B](f: A2 => Either[String, B], g: B => Either[String, A2]): Schema[B] =
    Schema.Transform[A2, B](self, f, g)

  def zip[A2 >: A, B](that: Schema[B]): Schema[(A2, B)] = Schema.Tuple(self, that)
}

object Schema {
  sealed case class Record[A](structure: Map[String, Schema[A]])      extends Schema[Map[String, A]]
  sealed case class Sequence[A](element: Schema[A])                   extends Schema[Chunk[A]]
  sealed case class Enumeration[A](structure: Map[String, Schema[A]]) extends Schema[Map[String, A]]
  sealed case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Schema[B]
  sealed case class Primitive[A](standardType: StandardType[A])    extends Schema[A]
  sealed case class Tuple[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]
  sealed case class Optional[A](codec: Schema[A])                  extends Schema[Option[A]]

  def apply[A](implicit codec: Schema[A]): Schema[A] = codec

  def caseClassN[A, Z](t1: (String, Schema[A]))(f: A => Z, g: Z => Option[A]): Schema[Z] =
    Schema
      .record(Map(t1))
      .transformOrFail(
        { (map: Map[String, A]) =>
          val v1 = map(t1._1)

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
      .record(Map[String, Schema[Any]](t1, t2))
      .transformOrFail(
        { (map: Map[String, Any]) =>
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
      .record(Map[String, Schema[Any]](t1, t2, t3))
      .transformOrFail(
        { (map: Map[String, Any]) =>
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
      { (map: Map[String, Any]) =>
        map.headOption.map {
          case ("Left", v)  => Right(Left(v.asInstanceOf[A]))
          case ("Right", v) => Right(Right(v.asInstanceOf[B]))
          case _            => Left("Expected left or right of sum")
        }.getOrElse(Left("Expected left or right of sum"))
      }, { (e: Either[A, B]) =>
        e match {
          case Left(v)  => Right(Map("Left"  -> v))
          case Right(v) => Right(Map("Right" -> v))
        }
      }
    )

  def enumeration[A](structure: Map[String, Schema[A]]): Schema[Map[String, A]] =
    Enumeration(structure)

  def first[A](codec: Schema[(A, Unit)]): Schema[A] =
    codec.transform[(A, Unit), A](_._1, a => (a, ()))

  implicit def list[A](implicit element: Schema[A]): Schema[List[A]] =
    sequence(element).transform((chunk: Chunk[A]) => chunk.toList, (list: List[A]) => Chunk.fromIterable(list))

  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType)

  def record[A](structure: Map[String, Schema[A]]): Schema[Map[String, A]] =
    Record(structure)

  implicit def sequence[A](implicit element: Schema[A]): Schema[Chunk[A]] =
    Sequence(element)

  implicit def set[A](implicit element: Schema[A]): Schema[Set[A]] =
    sequence(element).transform((chunk: Chunk[A]) => chunk.toSet, (set: Set[A]) => Chunk.fromIterable(set))

  def second[A](codec: Schema[(Unit, A)]): Schema[A] =
    codec.transform[(Unit, A), A](_._2, a => ((), a))

  implicit def vector[A](implicit element: Schema[A]): Schema[Vector[A]] =
    sequence(element).transform((chunk: Chunk[A]) => chunk.toVector, (vec: Vector[A]) => Chunk.fromIterable(vec))

  implicit def zipN[A, B](implicit c1: Schema[A], c2: Schema[B]): Schema[(A, B)] =
    c1.zip(c2)

  implicit def zipN[A, B, C](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C]): Schema[(A, B, C)] =
    c1.zip(c2)
      .zip(c3)
      .transform({ (el: ((A, B), C)) =>
        el match {
          case ((a, b), c) => (a, b, c)
        }
      }, { (el: (A, B, C)) =>
        el match {
          case (a, b, c) => ((a, b), c)
        }
      })

  implicit def zipN[A, B, C, D](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D]
  ): Schema[(A, B, C, D)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .transform(
        { (el: (((A, B), C), D)) =>
          (el._1._1._1, el._1._1._2, el._1._2, el._2)
        }, { (el: (A, B, C, D)) =>
          el match {
            case (a, b, c, d) => (((a, b), c), d)
          }
        }
      )

}
