package zio.schema.codec.xml

final case class XmlName(localName: String, prefix: Option[String] = None, namespace: Option[String] = None) {
  def qualifiedName: String = prefix.fold(localName)(p => s"$p:$localName")
}

object XmlName {
  def apply(localName: String): XmlName                    = new XmlName(localName, None, None)
  def apply(localName: String, namespace: String): XmlName = new XmlName(localName, None, Some(namespace))
}
