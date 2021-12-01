package dev.zio.schema.example.example8

sealed trait Json

object Json {
  final case class JStr(s: String)              extends Json
  final case class JNum(d: Double)              extends Json
  final case class JBool(b: Boolean)            extends Json
  final case class JArr(as: List[Json])         extends Json
  final case class JObj(map: Map[String, Json]) extends Json
}
