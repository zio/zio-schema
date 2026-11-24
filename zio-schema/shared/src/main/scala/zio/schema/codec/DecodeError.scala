package zio.schema.codec

import scala.collection.immutable.ListMap
import scala.util.control.NoStackTrace

import zio.schema.Schema.{Field, Record}
import zio.schema.validation.Validation
import zio.schema.{DynamicValue, Schema}
import zio.{Cause, Chunk}

/**
 * An error from decoding a value with a codec or from converting a
 * [[zio.schema.DynamicValue]] to a typed value. It is an `Exception` without a
 * stack trace, and `getMessage` returns [[message]].
 *
 * {{{
 * import zio.schema.codec.DecodeError
 *
 * val error: DecodeError =
 *   DecodeError.EmptyContent("no input") and DecodeError.ExtraFields("id", "unexpected field id")
 * // error.message == "no input and unexpected field id"
 * }}}
 */
sealed trait DecodeError extends Exception with NoStackTrace { self =>
  /** A human-readable description of the error. */
  def message: String

  override def getMessage(): String = message

  /** Combines this error and `that` into an [[DecodeError.And]]. */
  def and(that: DecodeError): DecodeError = DecodeError.And(self, that)

  /** Combines this error and `that` into an [[DecodeError.Or]]. */
  def or(that: DecodeError): DecodeError = DecodeError.Or(self, that)

}

/** The kinds of [[DecodeError]]. */
object DecodeError {
  /** Two errors that both occurred. */
  final case class And(left: DecodeError, right: DecodeError) extends DecodeError {
    /** The two messages joined with " and ". */
    def message: String = s"${left.message} and ${right.message}"
  }

  /** Two errors combined as alternatives. */
  final case class Or(left: DecodeError, right: DecodeError) extends DecodeError {
    /** The two messages joined with " or ". */
    override def message: String = s"${left.message} or ${right.message}"
  }

  /** The input has no value for the field `field` of the record `record`. */
  final case class RecordMissingField[R](record: Record[R], field: Field[R, _], message: String) extends DecodeError

  /** The input has no value for a field described by the schema `value`. */
  final case class MissingField(value: Schema[_], message: String) extends DecodeError

  /** A value does not match the schema `value`. */
  final case class MalformedField(value: Schema[_], message: String) extends DecodeError

  /** A value at the field path `path` does not match its schema. */
  final case class MalformedFieldWithPath(path: Chunk[String], message: String) extends DecodeError

  /** Reading the input failed, with the failure in `cause`. */
  final case class ReadError(cause: Cause[Any], message: String) extends DecodeError

  /** Reading the input at the field path `path` failed, with the failure in `cause`. */
  final case class ReadErrorWithPath(path: Chunk[String], cause: Cause[Any], message: String) extends DecodeError

  /** The value of the field `field` fails the validation `validation`. */
  final case class ValidationError(validation: Validation[_], field: Field[_, _], message: String) extends DecodeError

  /** The input has the field `fieldName`, which the schema does not define. */
  final case class ExtraFields(fieldName: String, message: String) extends DecodeError

  /** The input is empty, or empty where a value is required. */
  final case class EmptyContent(message: String) extends DecodeError

  /** The dynamic value `value` has a different shape than `schema`. */
  final case class CastError[A](value: DynamicValue, schema: Schema[A]) extends DecodeError {
    /** Returns "Failed to cast", the value, "to schema" and the schema. */
    def message: String = s"Failed to cast $value to schema $schema"
  }

  /** The enum `enumN` has no case with the id `key`. */
  final case class MissingCase(key: String, enumN: Schema.Enum[_]) extends DecodeError {
    /** Returns "Missing case", the key, "in enum" and the enum schema. */
    def message: String = s"Missing case $key in enum $enumN"
  }

  /** The record values `values` do not match the record fields `structure`. */
  final case class IncompatibleShape(values: ListMap[String, DynamicValue], structure: Chunk[Schema.Field[_, _]])
      extends DecodeError {
    /** Returns a message naming the values and the fields, ending in "have incompatible shape". */
    def message: String = s"Value $values and $structure have incompatible shape"
  }

  /** The decoder named `decoderName` cannot decode values of `schema`. */
  final case class UnsupportedSchema(schema: Schema[_], decoderName: String) extends DecodeError {
    /** Returns a message naming the schema and the decoder, "... is not supported by ...". */
    def message: String = s"Schema $schema is not supported by $decoderName"
  }

}
