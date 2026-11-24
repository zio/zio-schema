package zio.schema.codec

import scala.annotation.StaticAnnotation

import org.apache.avro.Schema.Field

/**
 * Annotations that change the Avro schema that [[AvroSchemaCodec]] generates
 * from a ZIO Schema. `AvroSchemaCodec.decode` also adds several of them to the
 * schemas it builds, to keep details of the Avro schema it read.
 *
 * The case class annotations go on a case class, a sealed trait or a field,
 * written as `@AvroAnnotations.name("...")`. `formatToString` and `error` are
 * objects, not classes, so they are attached with `Schema#annotate` instead.
 * `scale`, `precision`, `decimal`, `bytes` and `formatToString` are read from
 * the annotations of the value's primitive schema, not from the field, so they
 * are attached with `Schema#annotate` as well, as in
 * `Schema.primitive[java.math.BigDecimal].annotate(AvroAnnotations.scale(2))`.
 *
 * {{{
 * import zio.schema.{DeriveSchema, Schema}
 *
 * @AvroAnnotations.name("Customer")
 * @AvroAnnotations.namespace("com.example")
 * final case class User(@AvroAnnotations.doc("Full name") name: String)
 *
 * val schema: Schema[User] = DeriveSchema.gen[User]
 * val avro = AvroSchemaCodec.encode(schema)
 * // Right({"type":"record","name":"Customer","namespace":"com.example","fields":[{"name":"name","type":"string","doc":"Full name"}]})
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-schema/derivations/codecs/avro/ Avro codec]]
 */
object AvroAnnotations {

  /**
   * Sets the name that the generated Avro schema uses for a record, an enum, a
   * case of a sealed trait or a field, in place of the Scala name. For a record
   * or an enum, a name that does not match `[A-Za-z_][A-Za-z0-9_]*` makes
   * `AvroSchemaCodec.encode` return a `Left`, such as
   * `Left("Invalid Avro name: bad-name")`; for a field, it makes
   * `AvroSchemaCodec.encode` throw an `org.apache.avro.SchemaParseException`.
   */
  final case class name(name: String) extends StaticAnnotation

  /**
   * Sets the namespace of a generated Avro record, enum or fixed decimal. A
   * namespace that is not a dot-separated list of Avro names makes
   * `AvroSchemaCodec.encode` return a `Left`, such as
   * `Left("Invalid Avro namespace: 1bad")`.
   */
  final case class namespace(namespace: String) extends StaticAnnotation

  /**
   * Adds a `doc` string to a generated Avro record, enum, fixed decimal or
   * field.
   */
  final case class doc(doc: String) extends StaticAnnotation

  /**
   * Holds the aliases of an Avro record or field. `AvroSchemaCodec.decode` adds
   * it when the Avro schema it reads has aliases; `AvroSchemaCodec.encode` does
   * not write aliases into the generated schema.
   */
  final case class aliases(aliases: Set[String]) extends StaticAnnotation

  /**
   * Writes a sealed trait whose cases are all case objects as an Avro `enum`,
   * with one symbol per case. Without it, such a sealed trait becomes a union
   * of empty records.
   */
  final case class avroEnum() extends StaticAnnotation

  /**
   * Sets the scale of the Avro `decimal` logical type generated for a
   * `BigDecimal` or `BigInteger` schema. Without it, the scale is 24.
   * `AvroCodec` does not read it: it writes and reads decimal values with scale
   * 24, so `1.5` decodes as `1.500000000000000000000000` whatever the
   * annotation says.
   */
  final case class scale(scale: Int = 24) extends StaticAnnotation

  /**
   * Sets the precision of the Avro `decimal` logical type generated for a
   * `BigDecimal` schema. Without it, the precision is the larger of the scale
   * and 48. A `BigInteger` schema does not read it and uses the scale as the
   * precision.
   */
  final case class precision(precision: Int = 48) extends StaticAnnotation

  /**
   * Used to annotate a BigInteger or BigDecimal schema to indicate the Avro
   * type that holds the decimal logical type: `DecimalType.Bytes` (the default)
   * gives `bytes`, and `DecimalType.Fixed(size)` gives a `fixed` of `size`
   * bytes, named `Decimal_` followed by the precision and the scale (such as
   * `Decimal_10_2`) unless the schema has a `name` annotation.
   * `AvroSchemaCodec.encode` throws a `java.lang.IllegalArgumentException` when
   * `size` bytes cannot hold the precision (for example `Fixed(16)` with the
   * default precision 48), and `AvroCodec` encoding of a value whose schema has
   * `DecimalType.Fixed` throws a `java.lang.ClassCastException`.
   */
  final case class decimal(decimalType: DecimalType) extends StaticAnnotation

  /**
   * Used to annotate a binary schema
   * (`Schema.primitive(StandardType.BinaryType)`) to indicate the Avro type
   * encoding: `BytesType.Bytes` (the default) gives `bytes`, and
   * `BytesType.Fixed` gives a named `fixed`. `Schema[Chunk[Byte]]` is a
   * sequence schema, written as an array of `int`, so it does not read this
   * annotation. For a value whose schema has `BytesType.Fixed`, `AvroCodec`
   * encoding throws a `java.lang.ClassCastException`.
   */
  final case class bytes(bytesType: BytesType) extends StaticAnnotation

  /**
   * Marks a `LocalDate`, `LocalTime`, `LocalDateTime` or `Instant` schema as
   * written as an Avro string. `AvroSchemaCodec.decode` adds it to the schemas
   * it builds from Avro `string` schemas that carry the
   * `zio.schema.codec.stringType` property for these types.
   *
   * It does not change the output of `AvroSchemaCodec.encode` or `AvroCodec`:
   * these types are written as Avro strings holding the value's ISO-8601
   * `toString`, with or without it. It is an object, so it is attached with
   * `Schema#annotate`.
   */
  case object formatToString extends StaticAnnotation

  /**
   * Holds a precision (millisecond precision or microsecond precision) for the
   * Avro logical types 'Time', 'Timestamp' and 'Local timestamp'.
   * `AvroSchemaCodec.encode` and `AvroCodec` do not read it: `LocalTime`,
   * `Instant` and `LocalDateTime` values are written as Avro strings, not as
   * these logical types, with or without it.
   *
   * @param timeprecisionType
   *   the precision: either millisecond precision or microsecond precision
   */
  final case class timeprecision(timeprecisionType: TimePrecisionType) extends StaticAnnotation

  /**
   * Sets the default value of a field in the generated Avro schema. The value
   * is a Java object that Avro accepts as a default for the field's type, such
   * as `java.lang.Integer.valueOf(7)` for an `int` field; for a value that does
   * not match the type, `AvroSchemaCodec.encode` throws an
   * `org.apache.avro.AvroTypeException`. `AvroSchemaCodec.decode` adds it to
   * fields that have a default.
   */
  final case class default(javaDefaultObject: java.lang.Object) extends StaticAnnotation

  /**
   * Used to annotate a record in order to render it as an Avro error record:
   * the generated Avro schema has type `error` instead of `record`. It is an
   * object, so it is attached with `Schema#annotate`. `AvroSchemaCodec.decode`
   * adds it to records read from Avro `error` schemas.
   */
  case object error extends StaticAnnotation

  /**
   * Used to indicate the Avro field order of a record field: sets the `order`
   * attribute of the field in the generated Avro schema. Without it, the order
   * is ascending. `AvroSchemaCodec.decode` adds it to every field it reads.
   */
  final case class fieldOrder(fieldOrderType: FieldOrderType) extends StaticAnnotation

  /** The sort order of an Avro record field, as set by `fieldOrder`. */
  sealed trait FieldOrderType { self =>

    /** Returns the matching Apache Avro `Schema.Field.Order`. */
    def toAvroOrder: Field.Order = self match {
      case FieldOrderType.Ascending  => Field.Order.ASCENDING
      case FieldOrderType.Descending => Field.Order.DESCENDING
      case FieldOrderType.Ignore     => Field.Order.IGNORE
    }
  }

  /** The three Avro field orders. */
  object FieldOrderType {

    /** `Ascending`, the order of a field without a `fieldOrder` annotation. */
    val default: FieldOrderType = FieldOrderType.Ascending

    /**
     * Converts an Apache Avro field order to a `FieldOrderType`; `null` gives
     * `default`.
     */
    def fromAvroOrder(order: Field.Order): FieldOrderType = order match {
      case Field.Order.ASCENDING  => FieldOrderType.Ascending
      case Field.Order.DESCENDING => FieldOrderType.Descending
      case Field.Order.IGNORE     => FieldOrderType.Ignore
      case null                   => default
    }

    /** Sorts records by this field in ascending order. */
    case object Ascending extends FieldOrderType

    /** Sorts records by this field in descending order. */
    case object Descending extends FieldOrderType

    /** Leaves this field out when Avro compares records. */
    case object Ignore extends FieldOrderType
  }

  /** The Avro type that holds a decimal value, as set by `decimal`. */
  sealed trait DecimalType

  /** The two Avro types for decimals. */
  object DecimalType {

    /** `Bytes`, used when a schema has no `decimal` annotation. */
    val default: DecimalType = DecimalType.Bytes

    /** Stores the decimal as an Avro `fixed` of `size` bytes. */
    case class Fixed(size: Int) extends DecimalType

    /** Stores the decimal as Avro `bytes`. */
    case object Bytes extends DecimalType
  }

  /** The Avro type that holds a binary value, as set by `bytes`. */
  sealed trait BytesType

  /** The two Avro types for binary values. */
  object BytesType {

    /** `Bytes`, used when a schema has no `bytes` annotation. */
    val default: BytesType = BytesType.Bytes

    /**
     * Stores the value as an Avro `fixed` of `size` bytes with the given
     * `name`, `doc` and namespace `space`.
     */
    case class Fixed(size: Int, name: String, doc: String = "", space: String = "") extends BytesType

    /** Stores the value as Avro `bytes`. */
    case object Bytes extends BytesType
  }

  /** The precision held by `timeprecision`. */
  sealed trait TimePrecisionType

  /** The two time precisions. */
  object TimePrecisionType {

    /** `Millis`. */
    val default: TimePrecisionType = TimePrecisionType.Millis

    /** Millisecond precision. */
    case object Millis extends TimePrecisionType

    /** Microsecond precision. */
    case object Micros extends TimePrecisionType
  }
}
