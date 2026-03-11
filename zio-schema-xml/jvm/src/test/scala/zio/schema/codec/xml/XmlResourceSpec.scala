package zio.schema.codec.xml

import zio.test._

object XmlResourceSpec extends ZIOSpecDefault {

  private def loadResource(name: String): String = {
    val stream = getClass.getResourceAsStream(s"/xml/$name")
    if (stream == null) throw new IllegalArgumentException(s"Resource not found: /xml/$name")
    try {
      val source = scala.io.Source.fromInputStream(stream, "UTF-8")
      try source.mkString
      finally source.close()
    } finally stream.close()
  }

  private def roundTripXml(resourceName: String) = {
    val content = loadResource(resourceName)
    val parsed1 = XmlReader.read(content, ReaderConfig.default)
    parsed1 match {
      case Right(xml1) =>
        val written = XmlWriter.write(xml1, WriterConfig.default)
        val parsed2 = XmlReader.read(written, ReaderConfig.default)
        assertTrue(parsed2 == Right(xml1))
      case Left(err) =>
        assertTrue(false) ?? s"Failed to parse $resourceName: ${err.getMessage}"
    }
  }

  def spec: Spec[Any, Any] = suite("XmlResourceSpec")(
    suite("SOAP")(
      test("round-trip SOAP envelope")(roundTripXml("soap-envelope.xml")),
      test("SOAP envelope structure") {
        val xml = XmlReader.read(loadResource("soap-envelope.xml"), ReaderConfig.default)
        xml match {
          case Right(Xml.Element(name, _, children)) =>
            assertTrue(
              name.prefix.contains("soap"),
              name.localName == "Envelope",
              children.size == 2
            )
          case _ => assertTrue(false)
        }
      }
    ),
    suite("Maven POM")(
      test("round-trip Maven POM")(roundTripXml("maven-pom.xml")),
      test("POM structure") {
        val xml = XmlReader.read(loadResource("maven-pom.xml"), ReaderConfig.default)
        xml match {
          case Right(elem: Xml.Element) =>
            assertTrue(elem.name.localName == "project")
          case _ => assertTrue(false)
        }
      }
    ),
    suite("SVG")(
      test("round-trip SVG chart")(roundTripXml("svg-chart.xml")),
      test("SVG structure") {
        val xml = XmlReader.read(loadResource("svg-chart.xml"), ReaderConfig.default)
        xml match {
          case Right(Xml.Element(name, attrs, _)) =>
            assertTrue(
              name.localName == "svg",
              attrs.exists(_._1.localName == "viewBox")
            )
          case _ => assertTrue(false)
        }
      }
    ),
    suite("XHTML")(
      test("round-trip XHTML page")(roundTripXml("xhtml-page.xml")),
      test("XHTML structure") {
        val xml = XmlReader.read(loadResource("xhtml-page.xml"), ReaderConfig.default)
        xml match {
          case Right(Xml.Element(name, _, children)) =>
            assertTrue(
              name.localName == "html",
              children.size == 2
            )
          case _ => assertTrue(false)
        }
      }
    ),
    suite("Android Layout")(
      test("round-trip Android layout")(roundTripXml("android-layout.xml")),
      test("Android layout structure") {
        val xml = XmlReader.read(loadResource("android-layout.xml"), ReaderConfig.default)
        xml match {
          case Right(Xml.Element(name, attrs, children)) =>
            assertTrue(
              name.localName == "LinearLayout",
              attrs.nonEmpty,
              children.size == 3
            )
          case _ => assertTrue(false)
        }
      }
    ),
    suite("Spring Beans")(
      test("round-trip Spring beans config")(roundTripXml("spring-beans.xml")),
      test("Spring beans structure") {
        val xml = XmlReader.read(loadResource("spring-beans.xml"), ReaderConfig.default)
        xml match {
          case Right(Xml.Element(name, _, _)) =>
            assertTrue(name.localName == "beans")
          case _ => assertTrue(false)
        }
      }
    ),
    suite("GPX")(
      test("round-trip GPX track")(roundTripXml("gpx-track.xml")),
      test("GPX track structure") {
        val xml = XmlReader.read(loadResource("gpx-track.xml"), ReaderConfig.default)
        xml match {
          case Right(Xml.Element(name, attrs, _)) =>
            assertTrue(
              name.localName == "gpx",
              attrs.exists(_._1.localName == "version")
            )
          case _ => assertTrue(false)
        }
      }
    )
  )
}
