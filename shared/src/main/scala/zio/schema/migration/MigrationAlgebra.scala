package zio.schema.migration

import zio.schema._
import zio.schema.DynamicValue
import scala.quoted._

// 1. THE PURE DATA CORE (Serializable)
sealed trait MigrationAction {
  def reverse: MigrationAction
}

object MigrationAction {
  type NodePath = Vector[String]

  final case class AddField(path: NodePath, default: DynamicValue) extends MigrationAction {
    def reverse: MigrationAction = DropField(path)
  }

  final case class DropField(path: NodePath) extends MigrationAction {
    def reverse: MigrationAction = AddField(path, DynamicValue.Primitive(Primitive.Unit))
  }

  final case class RenameField(path: NodePath, newName: String) extends MigrationAction {
    def reverse: MigrationAction = {
      val parent = path.dropRight(1)
      val oldName = path.last
      RenameField(parent :+ newName, oldName)
    }
  }
}

// 2. THE MACRO BRIDGE (Compile-Time)
object MigrationMacros {
  import MigrationAction.NodePath

  inline def derivePath[A](inline selector: A => Any): NodePath = 
    ${ derivePathImpl('selector) }

  def derivePathImpl[A: Type](selector: Expr[A => Any])(using Quotes): Expr[NodePath] = {
    import quotes.reflect._
    def extract(term: Term): List[String] = term match {
      case Inlined(_, _, body) => extract(body)
      case Block(List(DefDef(_, _, _, Some(body))), _) => extract(body)
      case Select(qual, name) => extract(qual) :+ name
      case Ident(_) => Nil 
      case _ => report.errorAndAbort(s"Invalid selector: ${term.show}. Must be a direct path.")
    }
    val pathList = extract(selector.asTerm)
    Expr.ofList(pathList.map(Expr(_))).asExprOf[NodePath]
  }
}

// 3. THE BUILDER API
final case class MigrationBuilder[A, B](actions: Vector[MigrationAction] = Vector.empty) {
  inline def addField[V](inline selector: A => V, value: V)(implicit schema: Schema[V]): MigrationBuilder[A, B] = {
    val path = MigrationMacros.derivePath(selector)
    val dynVal = schema.toDynamic(value)
    copy(actions = actions :+ MigrationAction.AddField(path, dynVal))
  }
}
