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

      case (Generic.Enumeration((key, generic)), Schema.Enumeration(cases)) =>
        cases.get(key) match {
          case Some(schema) =>
            generic.toTypedValue(schema).asInstanceOf[Either[String, A]].map(key -> _)
          case None => Left(s"Failed to find case $key in enumeration $schema")
        }

      case (Generic.GenericLeft(generic), Schema.EitherSchema(schema1, _)) =>
        generic.toTypedValue(schema1).map(Left(_))

      case (Generic.GenericRight(generic), Schema.EitherSchema(_, schema1)) =>
        generic.toTypedValue(schema1).map(Right(_))

      case (Generic.GenericTuple(leftGeneric, rightGeneric), Schema.Tuple(leftSchema, rightSchema)) =>
        val typedLeft  = leftGeneric.toTypedValue(leftSchema)
        val typedRight = rightGeneric.toTypedValue(rightSchema)
        (typedLeft, typedRight) match {
          case (Left(e1), Left(e2)) => Left(s"Converting generic tuple to typed value failed with errors $e1 and $e2")
          case (_, Left(e))         => Left(e)
          case (Left(e), _)         => Left(e)
          case (Right(a), Right(b)) => Right(a -> b)
        }

      case (Generic.GenericSome(generic), Schema.Optional(schema: Schema[_])) =>
        generic.toTypedValue(schema).map(Some(_))

      case (Generic.GenericNone, Schema.Optional(_)) =>
        Right(None)

      case (Generic.Transform(generic), Schema.Transform(schema, f, _)) =>
        generic.toTypedValue(schema).flatMap(f)

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

      case Schema.Enumeration(map) =>
        val (key, v) = value
        map(key) match {
          case schema: Schema[a] =>
            val nestedValue = fromSchemaAndValue(schema, v.asInstanceOf[a])
            Generic.Enumeration(key -> nestedValue)
        }

      case Schema.EitherSchema(left, right) =>
        value match {
          case Left(a)  => Generic.GenericLeft(fromSchemaAndValue(left, a))
          case Right(b) => Generic.GenericRight(fromSchemaAndValue(right, b))
        }

      case Schema.Tuple(schemaA, schemaB) =>
        val (a, b) = value
        Generic.GenericTuple(fromSchemaAndValue(schemaA, a), fromSchemaAndValue(schemaB, b))

      case Schema.Optional(schema) =>
        value match {
          case Some(value) => Generic.GenericSome(fromSchemaAndValue(schema, value))
          case None        => Generic.GenericNone
        }

      case Schema.Transform(schema, _, g) =>
        g(value) match {
          case Left(_)  => ???
          case Right(a) => Generic.Transform(fromSchemaAndValue(schema, a))
        }

      case _ => ???
    }

  // final case class Integer(value: Int)                   extends Generic
  final case class Record(values: Map[String, Generic])  extends Generic
  final case class Enumeration(value: (String, Generic)) extends Generic

  // final case class Sequence[Col[_], A](schemaA: Schema[A], fromChunk: Chunk[A] => Col[A], toChunk: Col[A] => Chunk[A])
  //     extends Schema[Col[A]]

  // sealed case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
  //     extends Schema[B]

  // sealed case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
  //     extends Schema[B]

  sealed case class Primitive[A](value: A, standardType: StandardType[A]) extends Generic

  final case class GenericSome(value: Generic) extends Generic

  final case class Transform(value: Generic) extends Generic

  case object GenericNone extends Generic

  // final case class Fail[A](message: String) extends Schema[A]

  sealed case class GenericTuple(left: Generic, right: Generic) extends Generic

  final case class GenericLeft(value: Generic) extends Generic

  final case class GenericRight(value: Generic) extends Generic

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
