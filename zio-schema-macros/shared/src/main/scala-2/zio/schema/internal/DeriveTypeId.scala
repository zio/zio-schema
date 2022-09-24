package zio.schema

import scala.reflect.macros.blackbox

trait DeriveTypeId { self : TypeId.type =>
  def gen[T](): TypeId = macro DeriveTypeId.genTypeIdImpl[T]
}

object DeriveTypeId {

  def genTypeIdImpl[T: c.WeakTypeTag](c: blackbox.Context)(): c.Tree = {
    import c.universe._

    def cleanName(s: c.Symbol): String = s.name.decodedName.toString.trim

    def loop(current: c.Symbol, packageAcc: List[String], objAcc: List[String]): (String, List[String], List[String], String) =
      current match {
        case x if x.isPackage =>
          loop(x.owner, cleanName(x) :: packageAcc, objAcc)
        case x if x.isModuleClass || x.isClass =>
          loop(x.owner, packageAcc, cleanName(x) :: objAcc)
        case NoSymbol => {
          val pkg = packageAcc.filterNot(_ == "<root>")
          objAcc.reverse match {
            case head :: tail => 
              val fullyQualifiedName = ( pkg ++ objAcc).mkString(".")
              (fullyQualifiedName, pkg, tail.reverse, head)
            case Nil          => c.abort(c.enclosingPosition, "this shouldn't be possible - please file an issue!")
          }
        }

        case _ => loop(current.owner, packageAcc, objAcc)

      }
    weakTypeOf[T].typeSymbol match {
      case x if x.isModuleClass || x.isClass =>
        val info = loop(x, Nil, Nil)
         q"zio.schema.TypeId.registerTypeId(${info._1}, zio.Chunk.apply(..${info._2}), Chunk.apply(..${info._3}), ${info._4})"//Can't unquote zio.Chunk[String]
      case _ =>
        println(s"structural at ${c.enclosingPosition}")
         q"zio.schema.TypeId.Structural"
    }
  }
}
