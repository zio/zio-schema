package zio.schema.xml

import scala.xml._
import zio.schema._
import zio.{ZIO, Task}

object XmlCodec {
  def encode[A](schema: Schema[A], value: A): Elem = schema match {
    case Schema.Primitive(StandardType.StringType, _) =>
      <string>{value.toString}</string>
    case Schema.Primitive(StandardType.IntType, _) =>
      <int>{value.toString}</int>
    case Schema.Primitive(StandardType.BoolType, _) =>
      <boolean>{value.toString}</boolean>
    case Schema.Primitive(StandardType.DoubleType, _) =>
      <double>{value.toString}</double>
    case Schema.CaseClass1(_, field, construct, _) =>
      <caseClass>
        <field name={field.name}>{encode[Any](field.schema.asInstanceOf[Schema[Any]], field.get(value))}</field>
      </caseClass>
    case Schema.Enum1(_, case1, _) =>
      <enum>
        <case name={case1.id}>{encode[Any](case1.schema.asInstanceOf[Schema[Any]], case1.deconstruct(value))}</case>
      </enum>
    case Schema.EnumN(_, cases, _) =>
      <enum>
        {
          cases.toSeq.map { caseN =>
            <case name={caseN.id}>
              {encode[Any](caseN.schema.asInstanceOf[Schema[Any]], caseN.deconstruct(value))}
            </case>
          }
        }
      </enum>
    case Schema.Sequence(elementSchema, _, toChunk, _, _) =>
      <sequence>
        {
          toChunk(value).map { elem =>
            encode(elementSchema.asInstanceOf[Schema[Any]], elem)
          }
        }
      </sequence>
    case Schema.NonEmptySequence(elementSchema, _, toChunk, _, _) =>
      <nonEmptySequence>
        {
          toChunk(value).map { elem =>
            encode(elementSchema.asInstanceOf[Schema[Any]], elem)
          }
        }
      </nonEmptySequence>
    case schema if schema == Schema.dynamicValue =>
      <dynamic>{value.toString}</dynamic>
    case _ =>
      throw new UnsupportedOperationException("Unsupported schema type")
  }

  def decode[A](schema: Schema[A], xml: Elem): Task[A] = schema match {
    case Schema.Primitive(StandardType.StringType, _) =>
      ZIO.succeed(xml.text.asInstanceOf[A])
    case Schema.Primitive(StandardType.IntType, _) =>
      ZIO.succeed(xml.text.toInt.asInstanceOf[A])
    case Schema.Primitive(StandardType.BoolType, _) =>
      ZIO.succeed(xml.text.toBoolean.asInstanceOf[A])
    case Schema.Primitive(StandardType.DoubleType, _) =>
      ZIO.succeed(xml.text.toDouble.asInstanceOf[A])
    case Schema.CaseClass1(_, field, construct, _) =>
      val fieldValue = (xml \ "field").text
      decode(field.schema, XML.loadString(fieldValue)).map { decodedField =>
        construct(decodedField.asInstanceOf[field.FieldType])
      }
    case Schema.Enum1(_, case1, _) =>
      val caseValue = (xml \ "case").text
      decode(case1.schema, XML.loadString(caseValue)).map { decodedCase =>
        case1.construct(decodedCase.asInstanceOf[case1.FieldType])
      }
    case Schema.EnumN(_, cases, _) =>
      val caseNode = (xml \ "case").head
      val caseId = caseNode \@ "name"
      cases.toSeq.find(_.id == caseId) match {
        case Some(caseN) =>
          decode(caseN.schema.asInstanceOf[Schema[Any]], XML.loadString(caseNode.text))
            .map { decoded =>
              caseN.construct(decoded.asInstanceOf[caseN.FieldType])
            }
        case None =>
          ZIO.fail(new UnsupportedOperationException(s"Unknown case: $caseId"))
      }
    case Schema.Sequence(elementSchema, _, _, _, _) =>
      val elements = (xml \ "sequence" \ "_").map { node =>
        decode(elementSchema.asInstanceOf[Schema[Any]], node.asInstanceOf[Elem])
      }
      ZIO.collectAll(elements).map(_.asInstanceOf[A])
    case Schema.NonEmptySequence(elementSchema, _, _, _, _) =>
      val elements = (xml \ "nonEmptySequence" \ "_").map { node =>
        decode(elementSchema.asInstanceOf[Schema[Any]], node.asInstanceOf[Elem])
      }
      ZIO.collectAll(elements).map(_.asInstanceOf[A])
    case schema if schema == Schema.dynamicValue =>
      ZIO.succeed(xml.text.asInstanceOf[A])
    case _ =>
      ZIO.fail(new UnsupportedOperationException("Unsupported schema type"))
  }
}