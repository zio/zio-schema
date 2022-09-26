package zio.schema

import zio.Chunk
import zio.prelude._
import zio.schema.DeriveTypeId

import com.github.ghik.silencer.silent

//todo protect creation
object TypeId extends Subtype[String] with DeriveTypeId {

  private val cache = new java.util.concurrent.ConcurrentHashMap[TypeId, (Chunk[String], Chunk[String], String)]

  val Structural: TypeId = registerTypeId("zio.schema.TypeId.Structural", Chunk("zio", "schema"), Chunk("TypeId"), "Structural")
  
  @silent
  def registerTypeId(id: String, pkg: Chunk[String], obj: Chunk[String], tpe: String): TypeId = {
    cache.get(id) match {
      case null => 
        cache.put(wrap(id), (pkg, obj, tpe))
      case (pkg2, obj2, tpe2) =>
        if (pkg != pkg2 || obj != obj2 || tpe != tpe2)
          throw new IllegalArgumentException(s"Defect: type id $id already registered with different package, object or type name")
    }
    wrap(id)
  }

  implicit class TypeIdSyntax(private val self: TypeId) extends AnyVal {
    //todo needs to be safer if people can register their own type ids
    def packageName: Chunk[String] = cache.get(self)._1
    def objectName: Chunk[String] = cache.get(self)._2
    def typeName: String = cache.get(self)._3
  }
}






