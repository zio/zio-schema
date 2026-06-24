package zio.schema.codec

import scala.collection.immutable.ListMap
import scala.util.control.NoStackTrace

import zio.schema.Schema.{ Field, Record }
import zio.schema.validation.Validation
import zio.schema.{ DynamicValue, Schema }
import zio.{ Cause, Chunk }

sealed trait DecodeError extends Exception with NoStackTrace { self =>
  def message: String

  override def getMessage(): String = message

  def and(that: DecodeError): DecodeError = DecodeError.And(self, that)

  def or(that: DecodeError): DecodeError = DecodeError.Or(self, that)

}

object DecodeError {
  final case class And(left: DecodeError, right: DecodeError) extends DecodeError {
    def message: String = s"${left.message} and ${right.message}"
  }

  final case class Or(left: DecodeError, right: DecodeError) extends DecodeError {
    override def message: String = s"${left.message} or ${right.message}"
  }

  final case class RecordMissingField[R](record: Record[R], field: Field[R, _], message: String) extends DecodeError

  final case class MissingField(value: Schema[_], message: String) extends DecodeError

  final case class MalformedField(value: Schema[_], message: String) extends DecodeError

  final case class MalformedFieldWithPath(path: Chunk[String], message: String) extends DecodeError

  final case class ReadError(cause: Cause[Any], message: String) extends DecodeError

  final case class ReadErrorWithPath(path: Chunk[String], cause: Cause[Any], message: String) extends DecodeError

  final case class ValidationError(validation: Validation[_], field: Field[_, _], message: String) extends DecodeError

  final case class ExtraFields(fieldName: String, message: String) extends DecodeError

  final case class EmptyContent(message: String) extends DecodeError

  final case class CastError[A](value: DynamicValue, schema: Schema[A]) extends DecodeError {
    def message: String = s"Failed to cast $value to schema $schema"
  }

  final case class MissingCase(key: String, enumN: Schema.Enum[_]) extends DecodeError {
    def message: String = s"Missing case $key in enum $enumN"
  }

  final case class IncompatibleShape(values: ListMap[String, DynamicValue], structure: Chunk[Schema.Field[_, _]])
      extends DecodeError {
    def message: String = s"Value $values and $structure have incompatible shape"
  }

  final case class UnsupportedSchema(schema: Schema[_], decoderName: String) extends DecodeError {
    def message: String = s"Schema $schema is not supported by $decoderName"
  }

  def fromEither[A](value: Either[String, A]): Either[DecodeError, A] =
    value match {
      case Right(value) => Right(value)
      case Left(value)  => Left(ReadError(Cause.Empty, value))
    }

}
