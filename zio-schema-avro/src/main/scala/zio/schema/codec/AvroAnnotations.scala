package zio.schema.codec

import scala.annotation.StaticAnnotation

import org.apache.avro.Schema.Field

object AvroAnnotations {

  final case class name(name: String)            extends StaticAnnotation
  final case class namespace(namespace: String)  extends StaticAnnotation
  final case class doc(doc: String)              extends StaticAnnotation
  final case class aliases(aliases: Set[String]) extends StaticAnnotation
  final case class avroEnum()                    extends StaticAnnotation

  final case class scale(scale: Int = 24)         extends StaticAnnotation
  final case class precision(precision: Int = 48) extends StaticAnnotation

  /**
   * Used to annotate a BigInterger or BigDecimal type to indicate the logical
   * type encoding (avro bytes or avro fixed)
   */
  final case class decimal(decimalType: DecimalType) extends StaticAnnotation

  /**
   * Used to annotate a Bytes type to indicate the avro type encoding (avro
   * bytes or avro fixed)
   */
  final case class bytes(bytesType: BytesType) extends StaticAnnotation

  /**
   * Used to annotate fields of type LocalDate, LocalTime, LocalDateTime or
   * Instant in order to render them as a string using the given formatter
   * instead of rendering them as avro logical types.
   */
  case object formatToString extends StaticAnnotation

  /**
   * Used to indicate the precision (millisecond precision or microsecond
   * precision) of avro logical types 'Time', 'Timestamp' and 'Local timestamp'
   * @param timeprecisionType
   *   the precision: either millisecond precision or microsecond precision
   */
  final case class timeprecision(timeprecisionType: TimePrecisionType) extends StaticAnnotation

  final case class default(javaDefaultObject: java.lang.Object) extends StaticAnnotation

  /**
   * Used to annotate a record in order to render it as a avro error record
   */
  case object error extends StaticAnnotation

  /**
   * Used to indicate the avro field order of a record
   */
  final case class fieldOrder(fieldOrderType: FieldOrderType) extends StaticAnnotation

  sealed trait FieldOrderType { self =>

    def toAvroOrder: Field.Order = self match {
      case FieldOrderType.Ascending  => Field.Order.ASCENDING
      case FieldOrderType.Descending => Field.Order.DESCENDING
      case FieldOrderType.Ignore     => Field.Order.IGNORE
    }
  }

  object FieldOrderType {
    val default: FieldOrderType = FieldOrderType.Ascending

    def fromAvroOrder(order: Field.Order): FieldOrderType = order match {
      case Field.Order.ASCENDING  => FieldOrderType.Ascending
      case Field.Order.DESCENDING => FieldOrderType.Descending
      case Field.Order.IGNORE     => FieldOrderType.Ignore
      case null                   => default
    }
    case object Ascending  extends FieldOrderType
    case object Descending extends FieldOrderType
    case object Ignore     extends FieldOrderType
  }

  sealed trait DecimalType

  object DecimalType {
    val default: DecimalType = DecimalType.Bytes
    case class Fixed(size: Int) extends DecimalType
    case object Bytes           extends DecimalType
  }

  sealed trait BytesType

  object BytesType {
    val default: BytesType = BytesType.Bytes
    case class Fixed(size: Int, name: String, doc: String = "", space: String = "") extends BytesType
    case object Bytes                                                               extends BytesType
  }

  sealed trait TimePrecisionType

  object TimePrecisionType {
    val default: TimePrecisionType = TimePrecisionType.Millis
    case object Millis extends TimePrecisionType
    case object Micros extends TimePrecisionType
  }
}
