package zio.schema
// import zio.schema.Schema.Primitive

trait Generic { self =>

// sealed case class Record(structure: Map[String, Schema[_]]) extends Schema[Map[String, _]]

  def toTypedValue[A](schema: Schema[A]): Either[String, A] =
    (self, schema) match {
      case (Generic.Primitive(value, p), Schema.Primitive(p2)) if p == p2 =>
        Right(value.asInstanceOf[A])
      case (Generic.Record(generics), Schema.Record(schemas)) if (generics.keySet != schemas.keySet) =>
        Left(s"$generics and $schema have incompatible shape")
      case (Generic.Record(generics), Schema.Record(schemas)) =>
        val keys = generics.keySet
        keys.foldLeft[Either[String, Map[String, Any]]](Right(Map.empty)) {
          case (Right(map), key) =>
            val generic = generics(key)
            val schema  = schemas(key)
            generic.toTypedValue(schema) match {
              case Left(error)  => Left(error)
              case Right(value) => Right(map + (key -> value))
            }
          case (Left(string), _) => Left(string)
        }
      case _ =>
        Left(s"Failed to cast $self to schema $schema")
    }
}

object Generic {

  def fromSchemaAndValue[A](schema: Schema[A], value: A): Generic =
    schema match {
      case Schema.Primitive(p) => Generic.Primitive(value, p)
      case Schema.Record(schemas) =>
        val map: Map[String, _] = value
        Generic.Record(schemas.map {
          case (key, schema: Schema[a]) =>
            key -> fromSchemaAndValue(schema, map(key).asInstanceOf[a])
        })
      case _ => ???
    }

  // final case class Integer(value: Int)                   extends Generic
  final case class Record(values: Map[String, Generic])  extends Generic
  final case class Enumeration(value: (String, Generic)) extends Generic

  // sealed case class Record(structure: Map[String, Schema[_]]) extends Schema[Map[String, _]]

  // final case class Sequence[Col[_], A](schemaA: Schema[A], fromChunk: Chunk[A] => Col[A], toChunk: Col[A] => Chunk[A])
  //     extends Schema[Col[A]]

  // sealed case class Enumeration(structure: Map[String, Schema[_]]) extends Schema[Map[String, _]]

  // sealed case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
  //     extends Schema[B]

  sealed case class Primitive[A](value: A, standardType: StandardType[A]) extends Generic

  // sealed case class Optional[A](codec: Schema[A]) extends Schema[Option[A]]

  // final case class Fail[A](message: String) extends Schema[A]

  // sealed case class Tuple[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]

  // final case class EitherSchema[A, B](left: Schema[A], right: Schema[B]) extends Schema[Either[A, B]]

  // final case class Case[A <: Z, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A) {

  //   def deconstruct(z: Z): Option[A] =
  //     try {
  //       Some(unsafeDeconstruct(z))
  //     } catch { case _: IllegalArgumentException => None }
  // }

  // final case class Enum1[A <: Z, Z](case1: Case[A, Z])                                extends Schema[Z]
  // final case class Enum2[A1 <: Z, A2 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z]) extends Schema[Z]
  // final case class Enum3[A1 <: Z, A2 <: Z, A3 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z])
  //     extends Schema[Z]
  // final case class EnumN[Z](cases: Seq[Case[_, Z]]) extends Schema[Z]

  // sealed trait CaseClass[Z] extends Schema[Z] {
  //   def toRecord: Record
  // }
}
