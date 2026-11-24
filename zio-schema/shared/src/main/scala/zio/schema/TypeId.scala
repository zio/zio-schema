package zio.schema

import zio.Chunk

/**
 * A `TypeId` identifies the Scala type that a schema describes. Record and enum
 * schemas carry one (`Schema.Record#id`, `Schema.Enum#id`), `DeriveSchema.gen`
 * fills it in from the type's full name, and codecs read it to name types in
 * their output, for example a record name in Avro.
 *
 * A [[TypeId.Nominal]] holds the package names, the enclosing object names and
 * the type name. [[TypeId.Structural]] stands for a type without a name, such
 * as a record decoded into a `DynamicValue` without a schema. Build one from a
 * full name with [[TypeId.parse]].
 *
 * {{{
 * val id: TypeId     = TypeId.parse("com.example.Person")
 * // Nominal(Chunk(com,example),Chunk(),Person)
 * val name: String   = id.name // "Person"
 * val nested: TypeId = TypeId.parse("zio.schema.Schema.Field")
 * // Nominal(Chunk(zio,schema),Chunk(Schema),Field)
 * }}}
 */
sealed trait TypeId { self =>

  /**
   * Returns the simple type name of a [[TypeId.Nominal]], or `"Structural"` for
   * [[TypeId.Structural]].
   */
  def name: String = self match {
    case TypeId.Structural              => "Structural"
    case TypeId.Nominal(_, _, typeName) => typeName
  }
}

/**
 * The two kinds of [[TypeId]], and constructors that build them from names.
 */
object TypeId {

  /**
   * The id of a type that has no name, such as a record decoded into a
   * `DynamicValue` without a schema. Its `name` is `"Structural"`, and
   * [[TypeId.schema]] encodes it as the empty string.
   */
  case object Structural extends TypeId

  /**
   * The id of a named type: its package names (for example `com` and
   * `example`), the names of the objects it is nested in, and its own name.
   */
  final case class Nominal(packageName: Chunk[String], objectNames: Chunk[String], typeName: String) extends TypeId {

    /**
     * Returns the full dotted name, for example `"zio.schema.Schema.Field"`.
     */
    def fullyQualified: String = (packageName ++ objectNames ++ Chunk(typeName)).mkString(".")
  }

  /**
   * Returns a [[TypeId.Nominal]] with the given type name and no package or
   * object names. The name is not split on dots.
   */
  def fromTypeName(typeName: String): TypeId = Nominal(Chunk.empty, Chunk.empty, typeName)

  /**
   * Parses a dotted full name into a [[TypeId.Nominal]]. The last segment is
   * the type name, the segments before it that start with an upper-case letter
   * become object names, and the rest become package names. Returns
   * [[TypeId.Structural]] for the empty string.
   *
   * {{{
   * val person: TypeId = TypeId.parse("com.example.Person")
   * // Nominal(Chunk(com,example),Chunk(),Person)
   * val field: TypeId  = TypeId.parse("zio.schema.Schema.Field")
   * // Nominal(Chunk(zio,schema),Chunk(Schema),Field)
   * val empty: TypeId  = TypeId.parse("") // Structural
   * }}}
   */
  def parse(s: String): TypeId =
    if (s.isEmpty) Structural
    else
      s.split("\\.").toList.reverse match {
        case first :: remainder =>
          val objectNames  = remainder.takeWhile(_.headOption.fold(false)(_.isUpper)).reverse
          val packageNames = remainder.drop(objectNames.size).reverse
          Nominal(Chunk.fromIterable(packageNames), Chunk.fromIterable(objectNames), first)

        case Nil => Structural
      }

  /**
   * A schema that encodes a `TypeId` as a string: its full dotted name for a
   * [[TypeId.Nominal]] and the empty string for [[TypeId.Structural]], and
   * decodes with [[TypeId.parse]].
   */
  implicit lazy val schema: Schema[TypeId] =
    Schema[String].transform(
      parse,
      {
        case Structural       => ""
        case nominal: Nominal => nominal.fullyQualified
      }
    )
}
