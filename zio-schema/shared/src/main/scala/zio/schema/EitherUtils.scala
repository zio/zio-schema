package zio.schema

object EitherUtils {

  def attemptOrElse[E, A](a: => A, error: E): Either[E, A] =
    try {
      Right(a)
    } catch {
      case _: Throwable => Left(error)
    }
}
