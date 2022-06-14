package zio.schema.diff.matching

import zio.schema.Schema
import zio.schema.Schema._

import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.immutable.ListMap

import scala.language.implicitConversions

import zio.Chunk

import Leaf._
import Node._
import scala.annotation.tailrec

/**
 * A mutable representation of a value with its schema used for diffing, matching and patching.
 **/
sealed trait MatchTree[A] { self =>
  val id: Int
  val schema: Schema[_]
  override def toString(): String = show

  def show: String = {
    val sb = new StringBuilder
    showImpl(0, sb)
    sb.toString
  }

  private def showImpl(indent: Int, sb: StringBuilder, prefix: String = ""): Unit = {
    sb.append("\n")
    sb.append(self.id.toString.padTo(5, ' '))
    sb.append(" " * indent)
    if (prefix ne "") sb.append(prefix).append(": ")
    self match {
      case MatchTreeRoot(nodes, leaves, child, schema) => ???
      case EnumNode(id, parent, caseName, children, schema) =>
        sb.append(s"EnumNode(id, parent, $caseName, _, schema)")
        children.showImpl(indent + 2, sb)
      case RecordNode(id, parent, children, schema) =>
        sb.append(s"RecordNode(id, parent, _, schema)")
        children.foreach({ case (label, tree) => tree.showImpl(indent + 2, sb, label) })
      case SequenceNode(id, parent, children, schema) =>
        sb.append(s"SequenceNode(id, parent, _, schema)")
        children.foreach(tree => tree.showImpl(indent + 2, sb))
      case PrimitiveLeaf(id, parent, value, schema) =>
        sb.append(s"PrimitiveLeaf(id, parent, $value, schema)")

    }
  }
}

final case class MatchTreeRoot[A](
  protected val nodes: MutableMap[Int, Node[Any, Any, Any]],
  protected val leaves: MutableMap[Int, Leaf[Any]],
  protected var child: MatchTree[A],
  schema: Schema[A]
) extends MatchTree[A] {
  val id: Int = 0

}
sealed trait Child {
  var parent: MatchTree[_]
}

sealed trait Node[A, Col, Elem] extends MatchTree[A] with Child {
  protected var children: Col
}

sealed trait CollectionNode[A, C[_], Elem] extends Node[A, C[MatchTree[Elem]], Elem] {
  protected var children: C[MatchTree[Elem]]
}

object Node {

  final case class EnumNode[A, Elem](
    id: Int,
    var parent: MatchTree[_],
    caseName: String,
    protected var children: MatchTree[Elem],
    schema: Schema.Enum[A]
  ) extends Node[A, MatchTree[Elem], Elem] {}

  object EnumNode {

    def apply[A, B](
      id: Int,
      parent: MatchTree[_],
      caseName: String,
      child: (EnumNode[A, B]) => MatchTree[B],
      schema: Schema.Enum[A]
    ): EnumNode[A, B] = {
      val res = new EnumNode(id, parent, caseName, null.asInstanceOf[MatchTree[B]], schema)
      res.children = child(res)
      res
    }
  }

  final case class RecordNode[A](
    id: Int,
    var parent: MatchTree[_],
    protected var children: ListMap[String, MatchTree[Any]],
    schema: Schema.Record[A]
  ) extends Node[A, ListMap[String, MatchTree[Any]], Any]

  object RecordNode {

    def apply[A](
      id: Int,
      parent: MatchTree[_],
      child: (RecordNode[A]) => ListMap[String, MatchTree[Any]],
      schema: Schema.Record[A]
    ): RecordNode[A] = {
      val res = new RecordNode(id, parent, null.asInstanceOf[ListMap[String, MatchTree[Any]]], schema)
      res.children = child(res)
      res
    }
  }

  final case class SequenceNode[A, Col, Elem](
    id: Int,
    var parent: MatchTree[_],
    protected var children: Chunk[MatchTree[Elem]],
    schema: Schema.Sequence[Col, Elem, A]
  ) extends CollectionNode[A, Chunk, Elem]

  object SequenceNode {

    def apply[A, Col, Elem](
      id: Int,
      parent: MatchTree[_],
      children: (SequenceNode[A, Col, Elem]) => Chunk[MatchTree[Elem]],
      schema: Schema.Sequence[Col, Elem, A]
    ): SequenceNode[A, Col, Elem] = {
      val res = new SequenceNode(id, parent, Chunk.empty.asInstanceOf[Chunk[MatchTree[Elem]]], schema)
      res.children = children(res)
      res
    }
  }

}
sealed trait Leaf[A] extends MatchTree[A] with Child

object Leaf {
  final case class PrimitiveLeaf[A](id: Int, var parent: MatchTree[_], value: A, schema: Schema.Primitive[A])
      extends Leaf[A]
}

//prevents forgetting to increment the count
sealed protected class Counter {
  private var count: Int = 0
  //used via the companion object
  protected def increment: Int = {
    count += 1
    println(s"incrementing count to $count")
    count
  }
}

object Counter {
  def apply(): Counter = new Counter()

  implicit def toInt(c: Counter): Int = c.increment
}

object MatchTree {

  def fromValue[A](value: A)(implicit schema: Schema[A]): MatchTree[A] = {
    val i      = Counter()
    val leaves = MutableMap.empty[Int, Leaf[Any]]
    val nodes  = MutableMap.empty[Int, Node[Any, Any, Any]]

    def loop[A](parent: MatchTree[_], value: A, schema: Schema[A]): MatchTree[A] = {
      def buildEnum[A, B](caseN: Case[B, A], valueN: B, schema: Schema.Enum[A]): EnumNode[A, B] =
        EnumNode(i, parent, caseN.id, loop(_, valueN, caseN.codec), schema)

      //scalafmt: { maxColumn = 400 }
      schema match {

        case l @ Schema.Lazy(_) => loop(parent, value, l.schema)

        case p @ Schema.Primitive(s, _) => PrimitiveLeaf(i, parent, value, p)

        case Schema.GenericRecord(structure, _) => ???

        case e @ Schema.Enum1(case1, _) => buildEnum(case1, case1.deconstruct(value).get, e)

        case e @ Schema.Enum2(case1, case2, _) =>
          (case1.deconstruct(value), case2.deconstruct(value)) match {
            case (Some(v1), _) => buildEnum(case1, v1, e)
            case (_, Some(v2)) => buildEnum(case1, v2, e)
          }

        case e @ Schema.Enum3(case1, case2, case3, _) =>
          (case1.deconstruct(value), case2.deconstruct(value), case3.deconstruct(value)) match {
            case (Some(v1), _, _) => buildEnum(case1, v1, e)
            case (_, Some(v2), _) => buildEnum(case2, v2, e)
            case (_, _, Some(v3)) => buildEnum(case3, v3, e)
            //This should never happen unless someone manually builds an Enum and doesn't include all cases
            case _ => throw new IllegalArgumentException("Enum value not found in enum")
          }
        case e @ Schema.Enum4(case1, case2, case3, case4, _) =>
          (case1.deconstruct(value), case2.deconstruct(value), case3.deconstruct(value), case4.deconstruct(value)) match {
            case (Some(v1), _, _, _) => buildEnum(case1, v1, e)
            case (_, Some(v2), _, _) => buildEnum(case2, v2, e)
            case (_, _, Some(v3), _) => buildEnum(case3, v3, e)
            case (_, _, _, Some(v4)) => buildEnum(case4, v4, e)
            //This should never happen unless someone manually builds an Enum and doesn't include all cases
            case _ => throw new IllegalArgumentException("Enum value not found in enum")
          }
        case e @ Schema.Enum5(case1, case2, case3, case4, case5, _) =>
          (case1.deconstruct(value), case2.deconstruct(value), case3.deconstruct(value), case4.deconstruct(value), case5.deconstruct(value)) match {
            case (Some(v1), _, _, _, _) => buildEnum(case1, v1, e)
            case (_, Some(v2), _, _, _) => buildEnum(case2, v2, e)
            case (_, _, Some(v3), _, _) => buildEnum(case3, v3, e)
            case (_, _, _, Some(v4), _) => buildEnum(case4, v4, e)
            case (_, _, _, _, Some(v5)) => buildEnum(case5, v5, e)
            //This should never happen unless someone manually builds an Enum and doesn't include all cases
            case _ => throw new IllegalArgumentException("Enum value not found in enum")
          }

        case Schema.Fail(message, _) => ???

        case s: Schema.Sequence[col, e, _] =>
          SequenceNode(i, parent, ((self: SequenceNode[A, col, e]) => s.toChunk(value).map(loop(self, _, s.schemaA))), s.asInstanceOf)

        case Schema.MapSchema(ks: Schema[k], vs: Schema[v], _) => ???
        case Schema.SetSchema(as: Schema[a], _)                => ???
        case schema: Schema.EitherSchema[l, r]                 => ???
        case schema: Schema.Tuple[a, b]                        => ???
        case schema: Schema.Optional[a]                        => ???
        case Schema.Transform(schema, _, g, _, _)              => ???
        case Schema.Meta(ast, _)                               => ???

        case c @ Schema.CaseClass0(_, _) =>
          RecordNode(
            i,
            parent,
            (
              (self: RecordNode[A]) =>
                ListMap(
                  )
              ),
            c
          )

        case c @ Schema.CaseClass1(f, _, ext, _) =>
          RecordNode(
            i,
            parent,
            (
              (self: RecordNode[A]) =>
                ListMap(
                  f.label -> loop(self, ext(value), f.schema)
                )
              ),
            c
          )

        case c @ Schema.CaseClass2(f1, f2, _, ext1, ext2, _) =>
          RecordNode(
            i,
            parent,
            (
              (self: RecordNode[A]) =>
                ListMap(
                  f1.label -> loop(self, ext1(value), f1.schema),
                  f2.label -> loop(self, ext2(value), f2.schema)
                )
              ),
            c
          )
        case c @ Schema.CaseClass3(f1, f2, f3, _, ext1, ext2, ext3, _) =>
          RecordNode(
            i,
            parent,
            (
              (self: RecordNode[A]) =>
                ListMap(
                  f1.label -> loop(self, ext1(value), f1.schema),
                  f2.label -> loop(self, ext2(value), f2.schema),
                  f3.label -> loop(self, ext3(value), f3.schema)
                )
              ),
            c
          )
        case c @ Schema.CaseClass4(f1, f2, f3, f4, _, ext1, ext2, ext3, ext4, _) =>
          RecordNode(
            i,
            parent,
            (
              (self: RecordNode[A]) =>
                ListMap(
                  f1.label -> loop(self, ext1(value), f1.schema),
                  f2.label -> loop(self, ext2(value), f2.schema),
                  f3.label -> loop(self, ext3(value), f3.schema),
                  f4.label -> loop(self, ext4(value), f4.schema)
                )
              ),
            c
          )
        case c @ Schema.CaseClass5(f1, f2, f3, f4, f5, _, ext1, ext2, ext3, ext4, ext5, _) =>
          RecordNode(
            i,
            parent,
            (
              (self: RecordNode[A]) =>
                ListMap(
                  f1.label -> loop(self, ext1(value), f1.schema),
                  f2.label -> loop(self, ext2(value), f2.schema),
                  f3.label -> loop(self, ext3(value), f3.schema),
                  f4.label -> loop(self, ext4(value), f4.schema),
                  f5.label -> loop(self, ext5(value), f5.schema)
                )
              ),
            c
          )
        case Schema.Dynamic(_)        => ???
        case Schema.SemiDynamic(_, _) => ???

      }
    }
    loop(null.asInstanceOf[MatchTree[A]], value, schema)

  }

}
