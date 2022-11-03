package zio.schema.codec

import zio.{ Cause, Chunk }
import zio.schema.Schema
import zio.schema.Schema.{ Field, Record }
import zio.schema.validation.Validation

import scala.util.control.NoStackTrace

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

  final case class MalformedFieldWithPath(path: Chunk[Any], message: String) extends DecodeError

  final case class ReadError(cause: Cause[Any], message: String) extends DecodeError

  final case class ValidationError(validation: Validation[_], field: Field[_, _], message: String) extends DecodeError

  final case class ExtraFields(fieldName: String, message: String) extends DecodeError

  final case class EmptyContent(message: String) extends DecodeError
}
