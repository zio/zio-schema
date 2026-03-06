package zio.schema.codec

import scala.xml.{ Elem, Node, Text, XML }

import zio.Chunk
import zio.schema.DynamicValue
import zio.schema.Schema

object XmlCodec {

  final case class Configuration(rootNode: String)

  object Configuration {
    val default: Configuration = Configuration("value")
  }

  def encode[A](schema: Schema[A], value: A, config: Configuration = Configuration.default): String = {
    val dynamic = schema.toDynamic(value)
    val node    = encodeDynamic(config.rootNode, dynamic)
    node.toString()
  }

  def decode[A](schema: Schema[A], xml: String, config: Configuration = Configuration.default): Either[String, A] =
    try {
      val root    = XML.loadString(xml)
      val dynamic = decodeDynamic(root)
      schema.fromDynamic(dynamic).left.map(_.mkString("; "))
    } catch {
      case t: Throwable => Left(t.getMessage)
    }

  private def encodeDynamic(name: String, value: DynamicValue): Elem =
    value match {
      case DynamicValue.Record(_, values) =>
        Elem(null, name, scala.xml.Null, scala.xml.TopScope, true, values.toSeq.map { case (k, v) => encodeDynamic(k, v) }: _*)
      case DynamicValue.Sequence(values)  =>
        Elem(null, name, scala.xml.Null, scala.xml.TopScope, true, values.map(v => encodeDynamic("item", v)): _*)
      case DynamicValue.SomeValue(v)      => encodeDynamic(name, v)
      case DynamicValue.NoneValue         => Elem(null, name, scala.xml.Null, scala.xml.TopScope, true)
      case DynamicValue.Primitive(v, _)   => Elem(null, name, scala.xml.Null, scala.xml.TopScope, true, Text(String.valueOf(v)))
      case other                          => Elem(null, name, scala.xml.Null, scala.xml.TopScope, true, Text(other.toString))
    }

  private def decodeDynamic(node: Node): DynamicValue = {
    val children = node.child.collect { case e: Elem => e }

    if (children.isEmpty) {
      val text = node.text.trim
      if (text.isEmpty) DynamicValue.NoneValue
      else DynamicValue.Primitive(text, zio.schema.StandardType.StringType)
    } else {
      val grouped = children.groupBy(_.label)
      if (grouped.keySet == Set("item"))
        DynamicValue.Sequence(Chunk.fromIterable(children.map(decodeDynamic)))
      else
        DynamicValue.Record(
          zio.schema.TypeId.parse(node.label),
          scala.collection.immutable.ListMap.from(children.map(c => c.label -> decodeDynamic(c)))
        )
    }
  }
}
