package zio.schema

import scala.annotation.tailrec
import scala.deriving._
import scala.quoted._

/** Helpers for the Scala 3 derivation macros, bound to the macro's `Quotes` instance `q`. */
class ReflectionUtils[Q <: Quotes & Singleton](val q: Q) {
  /** Makes `q` the given `Quotes` instance for the code inside the class. */
  given q.type = q
  import q.reflect._

  /** Returns an expression that is `Some` of the implicit `F[A]` found at the expansion site, or `None` when there is none. */
  def summonOptional[F[_]: Type, A: Type](using Quotes): Expr[Option[F[A]]] =
    Expr.summon[F[A]] match {
      case Some(instance) => '{ Some($instance) }
      case None           => '{ None }
    }

  /** Whether a `scala.deriving.Mirror` describes a sum type or a product type. */
  enum MirrorType {
    case Sum
    case Product
  }

  /** Reads the `MirrorType` of a mirror. */
  object MirrorType {
    /** Returns `Product` for a `Mirror.Product` and `Sum` for a `Mirror.Sum`. */
    def from(mirror: Expr[scala.deriving.Mirror]): MirrorType =
      mirror match {
        case '{ $x: scala.deriving.Mirror.Product } => MirrorType.Product
        case '{ $x: scala.deriving.Mirror.Sum }     => MirrorType.Sum
      }
  }

  /** The type members of a `scala.deriving.Mirror`, read at compile time: the mirrored type, its element types and labels, and its kind. */
  case class Mirror(
    mirroredType: TypeRepr,
    monoType: TypeRepr,
    types: Seq[TypeRepr],
    label: String,
    labels: Seq[String],
    mirrorType: MirrorType
  )

  /** Builds a `Mirror` from a mirror expression or from a type. */
  object Mirror {
    /** Reads the type members of `mirror`, or returns `None` when its `MirroredType`, `MirroredMonoType` or `MirroredLabel` is missing. */
    def apply(mirror: Expr[scala.deriving.Mirror]): Option[Mirror] = {
      val mirrorTpe = mirror.asTerm.tpe.widen
      for {
        mt   <- findMemberType(mirrorTpe, "MirroredType")
        mmt  <- findMemberType(mirrorTpe, "MirroredMonoType")
        mets <- findMemberType(mirrorTpe, "MirroredElemTypes").map(tupleTypeElements(_)).orElse(Some(Seq.empty))
        ml   <- findMemberType(mirrorTpe, "MirroredLabel")
        mels <- findMemberType(mirrorTpe, "MirroredElemLabels").map { mels =>
                  tupleTypeElements(mels).map { case ConstantType(StringConstant(l)) => l }
                }.orElse(Some(Seq.empty))
      } yield {
        val ConstantType(StringConstant(ml0)) = ml: @unchecked
        Mirror(mt, mmt, mets, ml0, mels, MirrorType.from(mirror))
      }
    }

    /** Searches for the `scala.deriving.Mirror` of `tpe` and reads it, or returns `None` when the compiler finds none. */
    def apply(tpe: TypeRepr): Option[Mirror] = {
      val MirrorType = TypeRepr.of[scala.deriving.Mirror]

      val mtpe = Refinement(MirrorType, "MirroredType", TypeBounds(tpe, tpe))

      val instance = Implicits.search(mtpe) match {
        case iss: ImplicitSearchSuccess => Some(iss.tree.asExprOf[scala.deriving.Mirror])
        case _: ImplicitSearchFailure   => None
      }

      instance.flatMap(Mirror(_))
    }
  }

  /** Returns the element types of a tuple type written with `*:`, such as `Int *: String *: EmptyTuple`, in order. */
  def tupleTypeElements(tp: TypeRepr): List[TypeRepr] = {
    @tailrec def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tp match {
      case AppliedType(pairTpe, List(hd: TypeRepr, tl: TypeRepr)) => loop(tl, hd :: acc)
      case _                                                      => acc
    }
    loop(tp, Nil).reverse
  }

  /** Returns the lower bound of `tp` when it is a `TypeBounds`, and `tp` itself otherwise. */
  def low(tp: TypeRepr): TypeRepr = tp match {
    case tp: TypeBounds => tp.low
    case tp             => tp
  }

  /** Returns the type member `name` of a refined type, searching its parents and both sides of an intersection, or `None` when it has none. */
  def findMemberType(tp: TypeRepr, name: String): Option[TypeRepr] = tp match {
    case Refinement(_, `name`, tp) => Some(low(tp))
    case Refinement(parent, _, _)  => findMemberType(parent, name)
    case AndType(left, right)      => findMemberType(left, name).orElse(findMemberType(right, name))
    case _                         => None
  }
}
