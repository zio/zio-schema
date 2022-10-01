package zio.schema

import zio.Chunk

sealed trait TypeId { self =>

  def name: String = self match {
    case TypeId.Structural              => "Structural"
    case TypeId.Nominal(_, _, typeName) => typeName
  }
}

object TypeId {
  case object Structural extends TypeId
  final case class Nominal(packageName: Chunk[String], objectNames: Chunk[String], typeName: String) extends TypeId {
    def fullyQualified: String = (packageName ++ objectNames ++ Chunk(typeName)).mkString(".")
  }

  def fromTypeName(typeName: String): TypeId = Nominal(Chunk.empty, Chunk.empty, typeName)

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

  implicit val schema: Schema[TypeId] =
    Schema[String].transform(
      parse, {
        case Structural       => ""
        case nominal: Nominal => nominal.fullyQualified
      }
    )
}
