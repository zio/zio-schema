package zio.schema.codec.xml

/** The name of an XML element or attribute: a `localName` with an optional `prefix` and an optional namespace URI. */
final case class XmlName(localName: String, prefix: Option[String] = None, namespace: Option[String] = None) {
  /** Returns `prefix:localName`, or `localName` when there is no prefix. The namespace URI is not part of it. */
  def qualifiedName: String = prefix.fold(localName)(p => s"$p:$localName")
}

/** Constructors for names without a prefix. */
object XmlName {
  /** Creates a name with no prefix or namespace. */
  def apply(localName: String): XmlName                    = new XmlName(localName, None, None)
  /** Creates a name with the namespace URI `namespace` and no prefix. */
  def apply(localName: String, namespace: String): XmlName = new XmlName(localName, None, Some(namespace))
}
