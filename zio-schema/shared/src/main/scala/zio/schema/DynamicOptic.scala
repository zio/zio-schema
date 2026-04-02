package zio.schema

import zio.Chunk

sealed trait DynamicOptic { self =>
  def /(name: String): DynamicOptic = DynamicOptic.Field(self, name)
  def when(name: String): DynamicOptic = DynamicOptic.Case(self, name)
  def each: DynamicOptic = DynamicOptic.Each(self)
}

object DynamicOptic {
  case object Root extends DynamicOptic
  case class Field(parent: DynamicOptic, name: String) extends DynamicOptic
  case class Case(parent: DynamicOptic, name: String) extends DynamicOptic
  case class Each(parent: DynamicOptic) extends DynamicOptic

  def parse(path: String): DynamicOptic = {
    val parts = path.split('/').filter(_.nonEmpty)
    parts.foldLeft[DynamicOptic](Root) { (acc, part) =>
      if (part == "*") Each(acc)
      else if (part.startsWith("@")) Case(acc, part.substring(1))
      else Field(acc, part)
    }
  }
}
