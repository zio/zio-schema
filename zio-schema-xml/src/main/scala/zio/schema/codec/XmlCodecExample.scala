package zio.schema.codec

import zio.schema.DeriveSchema
import zio.schema.Schema

object XmlCodecExample {
  
  case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }
  
  case class Address(street: String, city: String, zipCode: String)
  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }
  
  case class Company(name: String, employees: List[Person], address: Address)
  object Company {
    implicit val schema: Schema[Company] = DeriveSchema.gen[Company]
  }
  
  def main(args: Array[String]): Unit = {
    // Create a sample company
    val company = Company(
      "Tech Corp",
      List(Person("Alice", 30), Person("Bob", 25)),
      Address("123 Main St", "Tech City", "12345")
    )
    
    // Create XML codec
    val codec = XmlCodec.schemaBasedBinaryCodec[Company]
    
    // Encode to XML
    val encoded = codec.encode(company)
    val xml = new String(encoded.toArray, "UTF-8")
    
    println("Encoded XML:")
    println(xml)
    println()
    
    // Decode from XML
    val decoded = codec.decode(encoded)
    
    println("Decoded result:")
    println(decoded)
    println()
    
    // Verify roundtrip
    decoded match {
      case Right(decodedCompany) =>
        println(s"Roundtrip successful: ${decodedCompany == company}")
      case Left(error) =>
        println(s"Decoding failed: $error")
    }
  }
}