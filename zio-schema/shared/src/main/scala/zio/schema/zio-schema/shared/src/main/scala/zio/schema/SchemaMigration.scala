package zio.json.internal

import scala.quoted._
import zio.json._
import zio.json.internal._
import scala.deriving.Mirror

object InlinedProductDecoder {
  inline def derived[A](using m: Mirror.ProductOf[A]): JsonDecoder[A] = ${ deriveImpl[A]('m) }

  def deriveImpl[A: Type](m: Expr[Mirror.ProductOf[A]])(using Quotes): Expr[JsonDecoder[A]] = {
    import quotes.reflect._

    val fields = TypeRepr.of[A].typeSymbol.caseFields
    val fieldNames = fields.map(_.name)
    
    // Compile-time resolving decoders
    val decoders = fields.map { f =>
      Expr.summon[JsonDecoder[?]] match {
        case Some(d) => d
        case None    => report.errorAndAbort(s"Missing JsonDecoder for ${f.name}")
      }
    }

    '{
      new JsonDecoder[A] {
        private val matrix = Array.from(${Expr(fieldNames)})

        def decodeJson(trace: List[JsonError], in: RetractReader): Either[JsonError, A] = {
          // No Array[Any] - Using local stack variables for speed
          ${
            val varDecls = fields.zipWithIndex.map { case (f, i) =>
              val tpe = TypeRepr.of[A].memberType(f).asType
              val default = tpe match {
                case '[Int] => '{ 0 }
                case '[Long] => '{ 0L }
                case '[Boolean] => '{ false }
                case '[Option[t]] => '{ None }
                case _ => '{ null.asInstanceOf[Any] }
              }
              Symbol.newVal(Symbol.spliceOwner, s"v$i", TypeRepr.of[Any], Flags.Mutable, Symbol.noSymbol) -> default
            }

            val varDeclsExprs = varDecls.map { case (sym, default) =>
              ValDef(sym, Some(default.asTerm))
            }

            val seenDecls = fields.zipWithIndex.map { case (_, i) =>
              Symbol.newVal(Symbol.spliceOwner, s"s$i", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol) -> '{ false }
            }

            val seenDeclsExprs = seenDecls.map { case (sym, default) =>
              ValDef(sym, Some(default.asTerm))
            }

            val matchCases = fields.zipWithIndex.map { case (f, i) =>
              val decoder = decoders(i)
              CaseDef(Literal(IntConstant(i)), None, '{
                ${Ref(varDecls(i)._1).asExpr} = $decoder.decodeJson(trace, in) match {
                  case Right(v) => v
                  case Left(e)  => return Left(e)
                }
                ${Ref(seenDecls(i)._1).asExpr} = true
              }.asTerm)
            } :+ CaseDef(Wildcard(), None, '{ Lexer.skipValue(trace, in) }.asTerm)

            val loop = '{
              if (Lexer.firstField(trace, in) != null) {
                var field: String = Lexer.lastFieldName
                while (field != null) {
                  val idx = Lexer.fieldIndex(field, matrix)
                  ${ Match('{ idx }.asTerm, matchCases).asExpr }
                  field = Lexer.nextField(trace, in)
                }
              }
            }.asTerm

            val validation = fields.zipWithIndex.map { case (f, i) =>
              val tpe = TypeRepr.of[A].memberType(f)
              if (!(tpe <:< TypeRepr.of[Option[?]])) {
                '{ if (!${Ref(seenDecls(i)._1).asExpr}) return Left(JsonError.Message("Missing: " + ${Expr(f.name)}) :: trace) }.asTerm
              } else '{}.asTerm
            }

            val result = '{ Right($m.fromProduct(Tuple.fromArray(Array(${
              val refs = varDecls.map(v => Ref(v._1).asExpr)
              Expr.ofList(refs)
            }*)).asInstanceOf[Product])) }.asTerm

            Block(varDeclsExprs ++ seenDeclsExprs ++ List(loop) ++ validation, result).asExprOf[Either[JsonError, A]]
          }
        }
      }
    }
  }
}
