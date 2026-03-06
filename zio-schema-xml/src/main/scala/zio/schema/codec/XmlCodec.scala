package zio.schema.codec

import scala.annotation._
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal
import scala.xml.{ Elem, Node, XML }

import zio.{ Chunk, ZIO }
import zio.schema._
import zio.schema.codec.XmlCodec.{ XmlDecoder, XmlEncoder }
import zio.stream.{ ZChannel, ZPipeline }

object XmlCodec {

  /**
   * Configuration for XML encoding/decoding.
   *
   * @param attributeNameFormat format for attribute names
   * @param elementNameFormat format for element names
   * @param useAttributes whether to encode primitives as attributes instead of child elements
   * @param explicitNulls whether to encode None/Unit as explicit elements
   */
  final case class Configuration(
    attributeNameFormat: NameFormat = NameFormat.Identity,
    elementNameFormat: NameFormat = NameFormat.Identity,
    useAttributes: Boolean = true,
    explicitNulls: Boolean = false
  )

  private val streamEncoderSeparator: Chunk[Node] = Chunk.empty

  /**
   * Create an XML encoder for a given schema.
   */
  def encoder[A](schema: Schema[A], configuration: Configuration = Configuration()): XmlEncoder[A] =
    new XmlEncoder(schema, configuration)

  /**
   * Create an XML decoder for a given schema.
   */
  def decoder[A](schema: Schema[A], configuration: Configuration = Configuration()): XmlDecoder[A] =
    new XmlDecoder(schema, configuration)

  /**
   * Create an XML codec for a given schema.
   */
  def codec[A](schema: Schema[A], configuration: Configuration = Configuration()): Codec[Node, Node, A] =
    new Codec[Node, Node, A] {
      override def encode(value: A): Node = encoder(schema, configuration).encode(value)

      override def streamEncoder: ZPipeline[Any, Nothing, A] =
        encoder(schema, configuration)., NodestreamEncoder

      override def decode(whole: Node): Either[DecodeError, A] = decoder(schema, configuration).decode(whole)

      override def streamDecoder: ZPipeline[Any, DecodeError, Node, A] =
        decoder(schema, configuration).streamDecoder
    }

  /**
   * XML Encoder implementation.
   */
  class XmlEncoder[-A](schema: Schema[A], configuration: Configuration) extends Encoder[Node, Node, A] {

    import XmlEncoder._

    override def encode(value: A): Node =
      encodeSchema(schema, value)

    override def streamEncoder: ZPipeline[Any, Nothing, A, Node] =
      ZPipeline.mapChunks[A, Node](_.map(encode))

    private def encodeSchema(s: Schema[_], value: Any): Node = {
      (s, value) match {
        case (Schema.Unit, _) =>
          makeElement("Unit")

        case (Schema.String, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[String])
          } else {
            makeElement("String", v.asInstanceOf[String])
          }

        case (Schema.Bool, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Boolean].toString)
          } else {
            makeElement("Bool", v.asInstanceOf[Boolean].toString)
          }

        case (Schema.Byte, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Byte].toString)
          } else {
            makeElement("Byte", v.asInstanceOf[Byte].toString)
          }

        case (Schema.Short, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Short].toString)
          } else {
            makeElement("Short", v.asInstanceOf[Short].toString)
          }

        case (Schema.Int, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Int].toString)
          } else {
            makeElement("Int", v.asInstanceOf[Int].toString)
          }

        case (Schema.Long, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Long].toString)
          } else {
            makeElement("Long", v.asInstanceOf[Long].toString)
          }

        case (Schema.Float, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Float].toString)
          } else {
            makeElement("Float", v.asInstanceOf[Float].toString)
          }

        case (Schema.Double, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Double].toString)
          } else {
            makeElement("Double", v.asInstanceOf[Double].toString)
          }

        case (Schema.Char, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[Char].toString)
          } else {
            makeElement("Char", v.asInstanceOf[Char].toString)
          }

        case (Schema.Binary, v) =>
          val bytes = v.asInstanceOf[Chunk[Byte]]
          makeElement("Binary", new String(bytes.toArray, "UTF-8"))

        case (Schema.UUID, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[java.util.UUID].toString)
          } else {
            makeElement("UUID", v.asInstanceOf[java.util.UUID].toString)
          }

        case (Schema.BigDecimal, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[java.math.BigDecimal].toString)
          } else {
            makeElement("BigDecimal", v.asInstanceOf[java.math.BigDecimal].toString)
          }

        case (Schema.BigInteger, v) =>
          if (configuration.useAttributes) {
            makeAttribute("value", v.asInstanceOf[java.math.BigInteger].toString)
          } else {
            makeElement("BigInteger", v.asInstanceOf[java.math.BigInteger].toString)
          }

        case (Schema.Tuple(left, right), v) =>
          val tuple = v.asInstanceOf[(Any, Any)]
          makeElement(
            "Tuple",
            encodeSchema(left, tuple._1),
            encodeSchema(right, tuple._2)
          )

        case (Schema.Map(keySchema, valueSchema), v) =>
          val map = v.asInstanceOf[Map[Any, Any]]
          val entries = map.map { case (k, ve) =>
            makeElement(
              "Entry",
              makeElement("Key", encodeSchema(keySchema, k)),
              makeElement("Value", encodeSchema(valueSchema, ve))
            )
          }.toSeq
          makeElement("Map", entries: _*)

        case (Schema.Set(schema), v) =>
          val set = v.asInstanceOf[Set[Any]]
          val elements = set.map(e => makeElement("Element", encodeSchema(schema, e))).toSeq
          makeElement("Set", elements: _*)

        case (Schema.Sequence(schema), v) =>
          val seq = v.asInstanceOf[Seq[Any]]
          val elements = seq.map(e => makeElement("Element", encodeSchema(schema, e))).toSeq
          makeElement("Sequence", elements: _*)

        case (Schema.Option(codec), v) =>
          v match {
            case None =>
              if (configuration.explicitNulls) {
                makeElement("None")
              } else {
                makeElement("Option")
              }
            case Some(value) =>
              makeElement("Some", encodeSchema(codec, value))
          }

        case (Schema.Either(left, right), v) =>
          v match {
            case Left(value)  => makeElement("Left", encodeSchema(left, value))
            case Right(value) => makeElement("Right", encodeSchema(right, value))
          }

        case (Schema.Lazy(_), v) =>
          encodeSchema(schema.asInstanceOf[Schema[A]], v)

        case (Schema.GenericRecord(structure), v) =>
          val record = v.asInstanceOf[zio.schema.GenericRecord]
          val fields = structure.fields.toList.map { field =>
            record.get(field.name) match {
              case Some(value) =>
                makeElement(
                  formatFieldName(field.name),
                  encodeSchema(field.schema, value)
                )
              case None if configuration.explicitNulls =>
                makeElement(formatFieldName(field.name), makeElement("None"))
              case None =>
                makeElement(formatFieldName(field.name))
            }
          }
          makeElement("Record", fields: _*)

        case (Schema.CaseClass1(_, field, construct, _), v) =>
          val value = construct(v)
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass1[_, _, _]].id)}",
            makeElement(field.name, encodeSchema(field.schema, field.get(value)))
          )

        case (Schema.CaseClass2(_, f0, f1, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass2[_, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value)))
          )

        case (Schema.CaseClass3(_, f0, f1, f2, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass3[_, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value)))
          )

        case (Schema.CaseClass4(_, f0, f1, f2, f3, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass4[_, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value)))
          )

        case (Schema.CaseClass5(_, f0, f1, f2, f3, f4, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass5[_, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value)))
          )

        case (Schema.CaseClass6(_, f0, f1, f2, f3, f4, f5, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass6[_, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value)))
          )

        case (Schema.CaseClass7(_, f0, f1, f2, f3, f4, f5, f6, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass7[_, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value)))
          )

        case (Schema.CaseClass8(_, f0, f1, f2, f3, f4, f5, f6, f7, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass8[_, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value)))
          )

        case (Schema.CaseClass9(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value)))
          )

        case (Schema.CaseClass10(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value)))
          )

        case (Schema.CaseClass11(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value)))
          )

        case (Schema.CaseClass12(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value)))
          )

        case (Schema.CaseClass13(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value)))
          )

        case (Schema.CaseClass14(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value)))
          )

        case (Schema.CaseClass15(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value)))
          )

        case (Schema.CaseClass16(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value))),
            makeElement(f15.name, encodeSchema(f15.schema, f15.get(value)))
          )

        case (Schema.CaseClass17(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value))),
            makeElement(f15.name, encodeSchema(f15.schema, f15.get(value))),
            makeElement(f16.name, encodeSchema(f16.schema, f16.get(value)))
          )

        case (Schema.CaseClass18(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value))),
            makeElement(f15.name, encodeSchema(f15.schema, f15.get(value))),
            makeElement(f16.name, encodeSchema(f16.schema, f16.get(value))),
            makeElement(f17.name, encodeSchema(f17.schema, f17.get(value)))
          )

        case (Schema.CaseClass19(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value))),
            makeElement(f15.name, encodeSchema(f15.schema, f15.get(value))),
            makeElement(f16.name, encodeSchema(f16.schema, f16.get(value))),
            makeElement(f17.name, encodeSchema(f17.schema, f17.get(value))),
            makeElement(f18.name, encodeSchema(f18.schema, f18.get(value)))
          )

        case (Schema.CaseClass20(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value))),
            makeElement(f15.name, encodeSchema(f15.schema, f15.get(value))),
            makeElement(f16.name, encodeSchema(f16.schema, f16.get(value))),
            makeElement(f17.name, encodeSchema(f17.schema, f17.get(value))),
            makeElement(f18.name, encodeSchema(f18.schema, f18.get(value))),
            makeElement(f19.name, encodeSchema(f19.schema, f19.get(value)))
          )

        case (Schema.CaseClass21(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value))),
            makeElement(f15.name, encodeSchema(f15.schema, f15.get(value))),
            makeElement(f16.name, encodeSchema(f16.schema, f16.get(value))),
            makeElement(f17.name, encodeSchema(f17.schema, f17.get(value))),
            makeElement(f18.name, encodeSchema(f18.schema, f18.get(value))),
            makeElement(f19.name, encodeSchema(f19.schema, f19.get(value))),
            makeElement(f20.name, encodeSchema(f20.schema, f20.get(value)))
          )

        case (Schema.CaseClass22(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, construct, _), v) =>
          val value = v
          makeElement(
            s"${typeIdToString(s.asInstanceOf[Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]].id)}",
            makeElement(f0.name, encodeSchema(f0.schema, f0.get(value))),
            makeElement(f1.name, encodeSchema(f1.schema, f1.get(value))),
            makeElement(f2.name, encodeSchema(f2.schema, f2.get(value))),
            makeElement(f3.name, encodeSchema(f3.schema, f3.get(value))),
            makeElement(f4.name, encodeSchema(f4.schema, f4.get(value))),
            makeElement(f5.name, encodeSchema(f5.schema, f5.get(value))),
            makeElement(f6.name, encodeSchema(f6.schema, f6.get(value))),
            makeElement(f7.name, encodeSchema(f7.schema, f7.get(value))),
            makeElement(f8.name, encodeSchema(f8.schema, f8.get(value))),
            makeElement(f9.name, encodeSchema(f9.schema, f9.get(value))),
            makeElement(f10.name, encodeSchema(f10.schema, f10.get(value))),
            makeElement(f11.name, encodeSchema(f11.schema, f11.get(value))),
            makeElement(f12.name, encodeSchema(f12.schema, f12.get(value))),
            makeElement(f13.name, encodeSchema(f13.schema, f13.get(value))),
            makeElement(f14.name, encodeSchema(f14.schema, f14.get(value))),
            makeElement(f15.name, encodeSchema(f15.schema, f15.get(value))),
            makeElement(f16.name, encodeSchema(f16.schema, f16.get(value))),
            makeElement(f17.name, encodeSchema(f17.schema, f17.get(value))),
            makeElement(f18.name, encodeSchema(f18.schema, f18.get(value))),
            makeElement(f19.name, encodeSchema(f19.schema, f19.get(value))),
            makeElement(f20.name, encodeSchema(f20.schema, f20.get(value))),
            makeElement(f21.name, encodeSchema(f21.schema, f21.get(value)))
          )

        case (Schema.Enum1(_, c0, _), v) =>
          val case0 = c0.asInstanceOf[Schema.Case[_, A]]
          makeElement(typeIdToString(case0.id), encodeSchema(case0.codec.asInstanceOf[Schema[Any]], case0.get(v)))

        case (Schema.Enum2(_, c0, c1, _), v) =>
          findEnumCase(v, c0, c1) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum3(_, c0, c1, c2, _), v) =>
          findEnumCase(v, c0, c1, c2) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum4(_, c0, c1, c2, c3, _), v) =>
          findEnumCase(v, c0, c1, c2, c3) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum5(_, c0, c1, c2, c3, c4, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum6(_, c0, c1, c2, c3, c4, c5, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum7(_, c0, c1, c2, c3, c4, c5, c6, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum8(_, c0, c1, c2, c3, c4, c5, c6, c7, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum9(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum10(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum11(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum12(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum13(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum14(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum15(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum16(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum17(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum18(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum19(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum20(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum21(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.Enum22(_, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _), v) =>
          findEnumCase(v, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21) match {
            case (caseN, codecN) =>
              makeElement(typeIdToString(caseN.id), encodeSchema(codecN.asInstanceOf[Schema[Any]], caseN.get(v)))
          }

        case (Schema.EnumN(id, cases, _), v) =>
          cases.find(c => c.get(v).isDefined) match {
            case Some(caseN) =>
              makeElement(
                typeIdToString(caseN.id),
                encodeSchema(caseN.codec.asInstanceOf[Schema[Any]], caseN.get(v).get)
              )
            case None =>
              throw new IllegalArgumentException(s"Could not find matching case for enum value: $v")
          }

        case (Schema.DynamicRecord, v) =>
          val record = v.asInstanceOf[DynamicRecord]
          val fields = record.fields.map { case (k, dv) =>
            makeElement(k, encodeDynamicValue(dv))
          }.toSeq
          makeElement("Dynamic", fields: _*)

        case (Schema.Dynamic, _) =>
          makeElement("Dynamic")

        case (Schema.Transform(codec, f, _), v) =>
          try {
            encodeSchema(codec, f(v))
          } catch {
            case NonFatal(e) => throw new RuntimeException(s"Failed to encode transform: ${e.getMessage}", e)
          }

        case (Schema.Suspend(codecF, _), v) =>
          encodeSchema(codecF(), v)

        case (Schema.Named(name, codec), v) =>
          makeElement(name, encodeSchema(codec, v))

        case _ =>
          throw new IllegalArgumentException(s"Unsupported schema type: ${s.getClass.getSimpleName}")
      }
    }

    private def encodeDynamicValue(dv: DynamicValue): Node = {
      dv match {
        case DynamicValue.Record(_, values) =>
          val fields = values.map { case (name, value) =>
            makeElement(name, encodeDynamicValue(value))
          }.toSeq
          makeElement("Record", fields: _*)

        case DynamicValue.Sequence(values) =>
          val elements = values.map(v => makeElement("Element", encodeDynamicValue(v))).toSeq
          makeElement("Sequence", elements: _*)

        case DynamicValue.SetValue(values) =>
          val elements = values.map(v => makeElement("Element", encodeDynamicValue(v))).toSeq
          makeElement("Set", elements: _*)

        case DynamicValue.Primitive(value, standardType) =>
          standardType match {
            case StandardType.UnitType   => makeElement("Unit")
            case StandardType.StringType => makeElement("String", value.asInstanceOf[String])
            case StandardType.BoolType   => makeElement("Bool", value.asInstanceOf[Boolean].toString)
            case StandardType.ByteType   => makeElement("Byte", value.asInstanceOf[Byte].toString)
            case StandardType.ShortType  => makeElement("Short", value.asInstanceOf[Short].toString)
            case StandardType.IntType    => makeElement("Int", value.asInstanceOf[Int].toString)
            case StandardType.LongType   => makeElement("Long", value.asInstanceOf[Long].toString)
            case StandardType.FloatType  => makeElement("Float", value.asInstanceOf[Float].toString)
            case StandardType.DoubleType => makeElement("Double", value.asInstanceOf[Double].toString)
            case StandardType.BinaryType =>
              makeElement("Binary", new String(value.asInstanceOf[Chunk[Byte]].toArray, "UTF-8"))
            case StandardType.CharType           => makeElement("Char", value.asInstanceOf[Char].toString)
            case StandardType.UUIDType           => makeElement("UUID", value.asInstanceOf[java.util.UUID].toString)
            case StandardType.BigDecimalType     => makeElement("BigDecimal", value.asInstanceOf[java.math.BigDecimal].toString)
            case StandardType.BigIntegerType     => makeElement("BigInteger", value.asInstanceOf[java.math.BigInteger].toString)
            case StandardType.DayOfWeekType      => makeElement("DayOfWeek", value.asInstanceOf[java.time.DayOfWeek].toString)
            case StandardType.MonthType          => makeElement("Month", value.asInstanceOf[java.time.Month].toString)
            case StandardType.MonthDayType       => makeElement("MonthDay", value.asInstanceOf[java.time.MonthDay].toString)
            case StandardType.PeriodType         => makeElement("Period", value.asInstanceOf[java.time.Period].toString)
            case StandardType.YearType           => makeElement("Year", value.asInstanceOf[java.time.Year].getValue.toString)
            case StandardType.YearMonthType      => makeElement("YearMonth", value.asInstanceOf[java.time.YearMonth].toString)
            case StandardType.ZoneIdType         => makeElement("ZoneId", value.asInstanceOf[java.time.ZoneId].toString)
            case StandardType.ZoneOffsetType     => makeElement("ZoneOffset", value.asInstanceOf[java.time.ZoneOffset].toString)
            case StandardType.DurationType       => makeElement("Duration", value.asInstanceOf[java.time.Duration].toString)
            case StandardType.InstantType        => makeElement("Instant", value.asInstanceOf[java.time.Instant].toString)
            case StandardType.LocalDateType      => makeElement("LocalDate", value.asInstanceOf[java.time.LocalDate].toString)
            case StandardType.LocalTimeType      => makeElement("LocalTime", value.asInstanceOf[java.time.LocalTime].toString)
            case StandardType.LocalDateTimeType  => makeElement("LocalDateTime", value.asInstanceOf[java.time.LocalDateTime].toString)
            case StandardType.OffsetTimeType     => makeElement("OffsetTime", value.asInstanceOf[java.time.OffsetTime].toString)
            case StandardType.OffsetDateTimeType => makeElement("OffsetDateTime", value.asInstanceOf[java.time.OffsetDateTime].toString)
            case StandardType.ZonedDateTimeType  => makeElement("ZonedDateTime", value.asInstanceOf[java.time.ZonedDateTime].toString)
            case StandardType.CurrencyType       => makeElement("Currency", value.asInstanceOf[java.util.Currency].toString)
          }

        case DynamicValue.Singleton(value) =>
          makeElement("Singleton", value.toString)

        case DynamicValue.SomeValue(value) =>
          makeElement("Some", encodeDynamicValue(value))

        case DynamicValue.NoneValue =>
          makeElement("None")

        case DynamicValue.Tuple(left, right) =>
          makeElement("Tuple", encodeDynamicValue(left), encodeDynamicValue(right))

        case DynamicValue.LeftValue(value) =>
          makeElement("Left", encodeDynamicValue(value))

        case DynamicValue.RightValue(value) =>
          makeElement("Right", encodeDynamicValue(value))

        case DynamicValue.Enumeration(_, _) =>
          makeElement("Enumeration")

        case DynamicValue.Dictionary(_) =>
          makeElement("Dictionary")

        case DynamicValue.BothValue(_, _) =>
          makeElement("Both")

        case DynamicValue.DynamicAst(_) =>
          makeElement("DynamicAst")

        case DynamicValue.Error(_) =>
          makeElement("Error")
      }
    }

    private def makeElement(label: String, children: Node*): Elem =
      Elem(null, label, scala.xml.Null, scala.xml.TopScope, false, children: _*)

    private def makeAttribute(label: String, value: String): Elem =
      Elem(null, label, scala.xml.Attribute(null, label, value, scala.xml.TopScope), scala.xml.TopScope, true)

    private def formatFieldName(name: String): String =
      configuration.elementNameFormat match {
        case NameFormat.Identity => name
        case NameFormat.LowerCamelCase => name.head.toLower + name.tail
        case NameFormat.UpperCamelCase => name.head.toUpper + name.tail
        case NameFormat.KebabCase => name.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
        case NameFormat.LowerSnakeCase => name.replaceAll("([a-z])([A-Z])", "$1_$2").toLowerCase
        case NameFormat.UpperSnakeCase => name.replaceAll("([a-z])([A-Z])", "$1_$2").toUpperCase
      }

    private def typeIdToString(typeId: TypeId): String =
      typeId.typeName.split('.').last

    @SuppressWarnings(Array("unchecked"))
    private def findEnumCase[V](
      v: V,
      cases: Schema.Case[_, V]*
    ): (Schema.Case[_, V], Schema[_]) =
      cases
        .find(c => c.get(v).isDefined)
        .map(c => (c, c.codec.asInstanceOf[Schema[Any]]))
        .getOrElse(throw new IllegalArgumentException(s"Could not find matching case for enum value: $v"))
  }

  object XmlEncoder {
    def apply[A](schema: Schema[A], configuration: Configuration = Configuration()): XmlEncoder[A] =
      new XmlEncoder(schema, configuration)
  }

  /**
   * XML Decoder implementation.
   */
  class XmlDecoder[+A](schema: Schema[A], configuration: Configuration) extends Decoder[Node, Node, A] {

    import XmlDecoder._

    override def decode(whole: Node): Either[DecodeError, A] =
      try {
        Right(decodeSchema(schema.asInstanceOf[Schema[Any]], whole).asInstanceOf[A])
      } catch {
        case e: Exception =>
          Left(DecodeError.ReadError(None, e.getMessage))
      }

    override def streamDecoder: ZPipeline[Any, DecodeError, Node, A] =
      ZPipeline.receiveAll[Any, DecodeError, Node, A] { chunks =>
        val combined = chunks.flatMap(_.toSeq).headOption.getOrElse(
          throw new IllegalArgumentException("No input data")
        )
        ZChannel.write(decode(combined).fold(Left(_), Right(_)))
      }

    private def decodeSchema(s: Schema[_], node: Node): Any = {
      (s, node) match {
        case (Schema.Unit, _) =>
          ()

        case (Schema.String, node) =>
          extractValue(node).getOrElse("")

        case (Schema.Bool, node) =>
          extractValue(node).map(_.toBoolean).getOrElse(false)

        case (Schema.Byte, node) =>
          extractValue(node).map(_.toByte).getOrElse(0.toByte)

        case (Schema.Short, node) =>
          extractValue(node).map(_.toShort).getOrElse(0.toShort)

        case (Schema.Int, node) =>
          extractValue(node).map(_.toInt).getOrElse(0)

        case (Schema.Long, node) =>
          extractValue(node).map(_.toLong).getOrElse(0L)

        case (Schema.Float, node) =>
          extractValue(node).map(_.toFloat).getOrElse(0.0f)

        case (Schema.Double, node) =>
          extractValue(node).map(_.toDouble).getOrElse(0.0)

        case (Schema.Char, node) =>
          extractValue(node).map(_.head).getOrElse('\0')

        case (Schema.Binary, node) =>
          extractValue(node).map(s => Chunk.fromArray(s.getBytes("UTF-8"))).getOrElse(Chunk.empty)

        case (Schema.UUID, node) =>
          extractValue(node).map(s => java.util.UUID.fromString(s)).getOrElse(java.util.UUID.randomUUID())

        case (Schema.BigDecimal, node) =>
          extractValue(node).map(s => new java.math.BigDecimal(s)).getOrElse(java.math.BigDecimal.ZERO)

        case (Schema.BigInteger, node) =>
          extractValue(node).map(s => new java.math.BigInteger(s)).getOrElse(java.math.BigInteger.ZERO)

        case (Schema.Tuple(left, right), node) =>
          val children = node.child.filter(_.label != "#PCDATA")
          if (children.length >= 2) {
            (decodeSchema(left, children(0)), decodeSchema(right, children(1)))
          } else {
            throw new IllegalArgumentException("Tuple requires at least 2 child elements")
          }

        case (Schema.Map(keySchema, valueSchema), node) =>
          val children = node.child.filter(_.label != "#PCDATA")
          children.map { entryNode =>
            val entryChildren = entryNode.child.filter(_.label != "#PCDATA")
            if (entryChildren.length >= 2) {
              (decodeSchema(keySchema, entryChildren(0)), decodeSchema(valueSchema, entryChildren(1)))
            } else {
              throw new IllegalArgumentException("Map Entry requires Key and Value elements")
            }
          }.toMap

        case (Schema.Set(elementSchema), node) =>
          val children = node.child.filter(_.label != "#PCDATA")
          children.map(child => decodeSchema(elementSchema, child)).toSet

        case (Schema.Sequence(elementSchema), node) =>
          val children = node.child.filter(_.label != "#PCDATA")
          children.map(child => decodeSchema(elementSchema, child)).toSeq

        case (Schema.Option(codec), node) =>
          val children = node.child.filter(_.label != "#PCDATA")
          if (children.isEmpty) {
            None
          } else if (children.head.label == "None") {
            None
          } else if (children.head.label == "Some") {
            Some(decodeSchema(codec, children.head))
          } else {
            Some(decodeSchema(codec, node))
          }

        case (Schema.Either(left, right), node) =>
          val children = node.child.filter(_.label != "#PCDATA")
          if (children.isEmpty) {
            throw new IllegalArgumentException("Either requires Left or Right element")
          } else if (children.head.label == "Left") {
            Left(decodeSchema(left, children.head))
          } else if (children.head.label == "Right") {
            Right(decodeSchema(right, children.head))
          } else {
            throw new IllegalArgumentException(s"Unknown Either variant: ${children.head.label}")
          }

        case (Schema.Lazy(s), node) =>
          decodeSchema(s.asInstanceOf[Schema[Any]], node)

        case (Schema.GenericRecord(structure), node) =>
          val fieldMap = ListMap.newBuilder[String, Any]
          val children = node.child.filter(_.label != "#PCDATA")
          children.foreach { fieldNode =>
            val fieldName = parseFieldName(fieldNode.label)
            val fieldSchema = structure.fields.find(_.name == fieldName)
            fieldSchema match {
              case Some(fs) =>
                fieldMap += (fieldName -> decodeSchema(fs.schema, fieldNode))
              case None =>
                // Skip unknown fields
            }
          }
          zio.schema.GenericRecord(fieldMap.result())

        case _ if isCaseClass(s) =>
          decodeCaseClass(s, node)

        case _ if isEnum(s) =>
          decodeEnum(s, node)

        case (Schema.DynamicRecord, node) =>
          val fieldMap = scala.collection.mutable.Map.empty[String, DynamicValue]
          val children = node.child.filter(_.label != "#PCDATA")
          children.foreach { fieldNode =>
            fieldMap += (fieldNode.label -> decodeDynamicValue(fieldNode))
          }
          DynamicRecord(fieldMap.toMap)

        case (Schema.Dynamic, node) =>
          decodeDynamicValue(node)

        case (Schema.Transform(codec, f, _), node) =>
          val decoded = decodeSchema(codec, node)
          f.asInstanceOf[Any => Any](decoded)

        case (Schema.Suspend(codecF, _), node) =>
          decodeSchema(codecF(), node)

        case (Schema.Named(_, codec), node) =>
          decodeSchema(codec, node)

        case _ =>
          throw new IllegalArgumentException(s"Unsupported schema type: ${s.getClass.getSimpleName}")
      }
    }

    private def decodeDynamicValue(node: Node): DynamicValue = {
      node.label match {
        case "Record" =>
          val fields = scala.collection.mutable.Map.empty[String, DynamicValue]
          node.child.filter(_.label != "#PCDATA").foreach { child =>
            fields += (child.label -> decodeDynamicValue(child))
          }
          DynamicValue.Record(TypeId.parse(node.label), fields.toMap)

        case "Sequence" | "Set" =>
          val elements = node.child.filter(_.label != "#PCDATA").map(decodeDynamicValue)
          if (node.label == "Set") {
            DynamicValue.SetValue(elements.toSet)
          } else {
            DynamicValue.Sequence(elements)
          }

        case "Tuple" =>
          val children = node.child.filter(_.label != "#PCDATA")
          if (children.length >= 2) {
            DynamicValue.Tuple(decodeDynamicValue(children(0)), decodeDynamicValue(children(1)))
          } else {
            DynamicValue.Sequence(Seq.empty)
          }

        case "Left" =>
          DynamicValue.LeftValue(decodeDynamicValue(node.child.filter(_.label != "#PCDATA").headOption.getOrElse(node)))

        case "Right" =>
          DynamicValue.RightValue(decodeDynamicValue(node.child.filter(_.label != "#PCDATA").headOption.getOrElse(node)))

        case "Some" =>
          DynamicValue.SomeValue(decodeDynamicValue(node.child.filter(_.label != "#PCDATA").headOption.getOrElse(node)))

        case "None" | "Option" =>
          DynamicValue.NoneValue

        case _ =>
          val value = extractValue(node)
          if (value.isDefined) {
            // Try to infer the type
            DynamicValue.Primitive(value.get, inferStandardType(value.get))
          } else {
            DynamicValue.DynamicAst(node.toString())
          }
      }
    }

    private def inferStandardType(value: String): StandardType[_] = {
      import StandardType._
      try {
        value.toLong
        try {
          value.toDouble
          DoubleType
        } catch {
          case _: NumberFormatException => LongType
        }
      } catch {
        case _: NumberFormatException =>
          if (value == "true" || value == "false") BoolType
          else StringType
      }
    }

    private def extractValue(node: Node): Option[String] = {
      if (node.attribute("value").isDefined) {
        Some(node.attribute("value").get.text)
      } else {
        val text = node.child.filter(_.label == "#PCDATA").map(_.text).mkString.trim
        if (text.isEmpty) None else Some(text)
      }
    }

    private def parseFieldName(label: String): String =
      configuration.elementNameFormat match {
        case NameFormat.Identity => label
        case NameFormat.LowerCamelCase => label
        case NameFormat.UpperCamelCase => label
        case NameFormat.KebabCase => label.split('-').map { s =>
          if (s.head.isUpper) s.head.toLower + s.tail else s
        }.mkString
        case NameFormat.LowerSnakeCase => label.split('_').map { s =>
          if (s.head.isUpper) s.head.toLower + s.tail else s
        }.mkString
        case NameFormat.UpperSnakeCase => label.split('_').map { s =>
          if (s.head.isLower) s.head.toUpper + s.tail else s
        }.mkString
      }

    @SuppressWarnings(Array("unchecked"))
    private def isCaseClass(s: Schema[_]): Boolean =
      s match {
        case _: Schema.CaseClass1[_, _, _] => true
        case _: Schema.CaseClass2[_, _, _, _] => true
        case _: Schema.CaseClass3[_, _, _, _, _] => true
        case _: Schema.CaseClass4[_, _, _, _, _, _] => true
        case _: Schema.CaseClass5[_, _, _, _, _, _, _] => true
        case _: Schema.CaseClass6[_, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass7[_, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass8[_, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _ => false
      }

    @SuppressWarnings(Array("unchecked"))
    private def isEnum(s: Schema[_]): Boolean =
      s match {
        case _: Schema.Enum1[_, _] => true
        case _: Schema.Enum2[_, _, _] => true
        case _: Schema.Enum3[_, _, _, _] => true
        case _: Schema.Enum4[_, _, _, _, _] => true
        case _: Schema.Enum5[_, _, _, _, _, _] => true
        case _: Schema.Enum6[_, _, _, _, _, _, _] => true
        case _: Schema.Enum7[_, _, _, _, _, _, _, _] => true
        case _: Schema.Enum8[_, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum9[_, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum10[_, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum11[_, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum12[_, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum13[_, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.Enum22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => true
        case _: Schema.EnumN[_, _] => true
        case _ => false
      }

    @SuppressWarnings(Array("unchecked"))
    private def decodeCaseClass(s: Schema[_], node: Node): Any = {
      val children = node.child.filter(_.label != "#PCDATA").toList

      def getFieldValue(field: Schema.Field[_], index: Int): Any = {
        if (index < children.length) {
          decodeSchema(field.schema, children(index))
        } else {
          null
        }
      }

      s match {
        case c: Schema.CaseClass1[a0, z] =>
          val f0 = c.field0.asInstanceOf[Schema.Field[Any]]
          c.construct(getFieldValue(f0, 0).asInstanceOf[c.A0]).asInstanceOf[Any]

        case c: Schema.CaseClass2[a0, a1, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass3[a0, a1, a2, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass4[a0, a1, a2, a3, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass5[a0, a1, a2, a3, a4, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass6[a0, a1, a2, a3, a4, a5, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass7[a0, a1, a2, a3, a4, a5, a6, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass8[a0, a1, a2, a3, a4, a5, a6, a7, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass9[a0, a1, a2, a3, a4, a5, a6, a7, a8, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass10[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass11[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass12[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass13[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass14[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass15[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass16[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          val f15 = c.f15.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14],
            getFieldValue(f15, 15).asInstanceOf[c.A15]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass17[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          val f15 = c.f15.asInstanceOf[Schema.Field[Any]]
          val f16 = c.f16.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14],
            getFieldValue(f15, 15).asInstanceOf[c.A15],
            getFieldValue(f16, 16).asInstanceOf[c.A16]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass18[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          val f15 = c.f15.asInstanceOf[Schema.Field[Any]]
          val f16 = c.f16.asInstanceOf[Schema.Field[Any]]
          val f17 = c.f17.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14],
            getFieldValue(f15, 15).asInstanceOf[c.A15],
            getFieldValue(f16, 16).asInstanceOf[c.A16],
            getFieldValue(f17, 17).asInstanceOf[c.A17]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass19[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          val f15 = c.f15.asInstanceOf[Schema.Field[Any]]
          val f16 = c.f16.asInstanceOf[Schema.Field[Any]]
          val f17 = c.f17.asInstanceOf[Schema.Field[Any]]
          val f18 = c.f18.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14],
            getFieldValue(f15, 15).asInstanceOf[c.A15],
            getFieldValue(f16, 16).asInstanceOf[c.A16],
            getFieldValue(f17, 17).asInstanceOf[c.A17],
            getFieldValue(f18, 18).asInstanceOf[c.A18]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass20[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          val f15 = c.f15.asInstanceOf[Schema.Field[Any]]
          val f16 = c.f16.asInstanceOf[Schema.Field[Any]]
          val f17 = c.f17.asInstanceOf[Schema.Field[Any]]
          val f18 = c.f18.asInstanceOf[Schema.Field[Any]]
          val f19 = c.f19.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14],
            getFieldValue(f15, 15).asInstanceOf[c.A15],
            getFieldValue(f16, 16).asInstanceOf[c.A16],
            getFieldValue(f17, 17).asInstanceOf[c.A17],
            getFieldValue(f18, 18).asInstanceOf[c.A18],
            getFieldValue(f19, 19).asInstanceOf[c.A19]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass21[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          val f15 = c.f15.asInstanceOf[Schema.Field[Any]]
          val f16 = c.f16.asInstanceOf[Schema.Field[Any]]
          val f17 = c.f17.asInstanceOf[Schema.Field[Any]]
          val f18 = c.f18.asInstanceOf[Schema.Field[Any]]
          val f19 = c.f19.asInstanceOf[Schema.Field[Any]]
          val f20 = c.f20.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14],
            getFieldValue(f15, 15).asInstanceOf[c.A15],
            getFieldValue(f16, 16).asInstanceOf[c.A16],
            getFieldValue(f17, 17).asInstanceOf[c.A17],
            getFieldValue(f18, 18).asInstanceOf[c.A18],
            getFieldValue(f19, 19).asInstanceOf[c.A19],
            getFieldValue(f20, 20).asInstanceOf[c.A20]
          ).asInstanceOf[Any]

        case c: Schema.CaseClass22[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, z] =>
          val f0 = c.f0.asInstanceOf[Schema.Field[Any]]
          val f1 = c.f1.asInstanceOf[Schema.Field[Any]]
          val f2 = c.f2.asInstanceOf[Schema.Field[Any]]
          val f3 = c.f3.asInstanceOf[Schema.Field[Any]]
          val f4 = c.f4.asInstanceOf[Schema.Field[Any]]
          val f5 = c.f5.asInstanceOf[Schema.Field[Any]]
          val f6 = c.f6.asInstanceOf[Schema.Field[Any]]
          val f7 = c.f7.asInstanceOf[Schema.Field[Any]]
          val f8 = c.f8.asInstanceOf[Schema.Field[Any]]
          val f9 = c.f9.asInstanceOf[Schema.Field[Any]]
          val f10 = c.f10.asInstanceOf[Schema.Field[Any]]
          val f11 = c.f11.asInstanceOf[Schema.Field[Any]]
          val f12 = c.f12.asInstanceOf[Schema.Field[Any]]
          val f13 = c.f13.asInstanceOf[Schema.Field[Any]]
          val f14 = c.f14.asInstanceOf[Schema.Field[Any]]
          val f15 = c.f15.asInstanceOf[Schema.Field[Any]]
          val f16 = c.f16.asInstanceOf[Schema.Field[Any]]
          val f17 = c.f17.asInstanceOf[Schema.Field[Any]]
          val f18 = c.f18.asInstanceOf[Schema.Field[Any]]
          val f19 = c.f19.asInstanceOf[Schema.Field[Any]]
          val f20 = c.f20.asInstanceOf[Schema.Field[Any]]
          val f21 = c.f21.asInstanceOf[Schema.Field[Any]]
          c.construct(
            getFieldValue(f0, 0).asInstanceOf[c.A0],
            getFieldValue(f1, 1).asInstanceOf[c.A1],
            getFieldValue(f2, 2).asInstanceOf[c.A2],
            getFieldValue(f3, 3).asInstanceOf[c.A3],
            getFieldValue(f4, 4).asInstanceOf[c.A4],
            getFieldValue(f5, 5).asInstanceOf[c.A5],
            getFieldValue(f6, 6).asInstanceOf[c.A6],
            getFieldValue(f7, 7).asInstanceOf[c.A7],
            getFieldValue(f8, 8).asInstanceOf[c.A8],
            getFieldValue(f9, 9).asInstanceOf[c.A9],
            getFieldValue(f10, 10).asInstanceOf[c.A10],
            getFieldValue(f11, 11).asInstanceOf[c.A11],
            getFieldValue(f12, 12).asInstanceOf[c.A12],
            getFieldValue(f13, 13).asInstanceOf[c.A13],
            getFieldValue(f14, 14).asInstanceOf[c.A14],
            getFieldValue(f15, 15).asInstanceOf[c.A15],
            getFieldValue(f16, 16).asInstanceOf[c.A16],
            getFieldValue(f17, 17).asInstanceOf[c.A17],
            getFieldValue(f18, 18).asInstanceOf[c.A18],
            getFieldValue(f19, 19).asInstanceOf[c.A19],
            getFieldValue(f20, 20).asInstanceOf[c.A20],
            getFieldValue(f21, 21).asInstanceOf[c.A21]
          ).asInstanceOf[Any]

        case _ =>
          throw new IllegalArgumentException(s"Unknown case class schema: ${s.getClass.getName}")
      }
    }

    @SuppressWarnings(Array("unchecked"))
    private def decodeEnum(s: Schema[_], node: Node): Any = {
      val children = node.child.filter(_.label != "#PCDATA")

      def findAndDecodeCase(cases: Schema.Case[_, _]*): Any = {
        cases.find(c => c.id.typeName.endsWith(node.label)) match {
          case Some(c) =>
            val codec = c.codec.asInstanceOf[Schema[Any]]
            if (children.isEmpty) {
              c.construct(None).asInstanceOf[Any]
            } else {
              c.construct(Some(decodeSchema(codec, children.head))).asInstanceOf[Any]
            }
          case None =>
            throw new IllegalArgumentException(s"Could not find matching case for: ${node.label}")
        }
      }

      s match {
        case c: Schema.Enum1[a, z] =>
          findAndDecodeCase(c.case0)

        case c: Schema.Enum2[a0, a1, z] =>
          findAndDecodeCase(c.case0, c.case1)

        case c: Schema.Enum3[a0, a1, a2, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2)

        case c: Schema.Enum4[a0, a1, a2, a3, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3)

        case c: Schema.Enum5[a0, a1, a2, a3, a4, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4)

        case c: Schema.Enum6[a0, a1, a2, a3, a4, a5, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5)

        case c: Schema.Enum7[a0, a1, a2, a3, a4, a5, a6, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6)

        case c: Schema.Enum8[a0, a1, a2, a3, a4, a5, a6, a7, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7)

        case c: Schema.Enum9[a0, a1, a2, a3, a4, a5, a6, a7, a8, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8)

        case c: Schema.Enum10[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9)

        case c: Schema.Enum11[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10)

        case c: Schema.Enum12[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11)

        case c: Schema.Enum13[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12)

        case c: Schema.Enum14[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13)

        case c: Schema.Enum15[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14)

        case c: Schema.Enum16[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14, c.case15)

        case c: Schema.Enum17[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14, c.case15, c.case16)

        case c: Schema.Enum18[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14, c.case15, c.case16, c.case17)

        case c: Schema.Enum19[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14, c.case15, c.case16, c.case17, c.case18)

        case c: Schema.Enum20[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14, c.case15, c.case16, c.case17, c.case18, c.case19)

        case c: Schema.Enum21[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14, c.case15, c.case16, c.case17, c.case18, c.case19, c.case20)

        case c: Schema.Enum22[a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, z] =>
          findAndDecodeCase(c.case0, c.case1, c.case2, c.case3, c.case4, c.case5, c.case6, c.case7, c.case8, c.case9, c.case10, c.case11, c.case12, c.case13, c.case14, c.case15, c.case16, c.case17, c.case18, c.case19, c.case20, c.case21)

        case c: Schema.EnumN[cases, z] =>
          findAndDecodeCase(c.cases.toSeq: _*)

        case _ =>
          throw new IllegalArgumentException(s"Unknown enum schema: ${s.getClass.getName}")
      }
    }
  }

  object XmlDecoder {
    def apply[A](schema: Schema[A], configuration: Configuration = Configuration()): XmlDecoder[A] =
      new XmlDecoder(schema, configuration)
  }

}
