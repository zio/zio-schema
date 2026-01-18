package zio.schema.codec

import zio._
import zio.test._
import java.nio.file.{Files, Paths}
import scala.io.Source

object MigrationVerificationSpec extends ZIOSpecDefault {

  // এই ফাংশনটি স্মার্টলি ফাইল খুঁজে বের করবে
  def findThriftCodecFile(): Task[String] = ZIO.attempt {
    
    // FIX: java.lang.System ব্যবহার করা হলো যাতে zio.System এর সাথে কনফ্লিক্ট না হয়
    val userDir = java.lang.System.getProperty("user.dir")
    
    // আপনার প্রজেক্ট স্ট্রাকচার অনুযায়ী সম্ভাব্য পাথগুলো
    val possiblePaths = List(
      // অপশন ১: রুট থেকে রান হলে
      Paths.get(userDir, "schema-thrift", "src", "main", "scala", "zio", "schema", "codec", "ThriftCodec.scala"),
      
      // অপশন ২: সাব-মডিউল ফোল্ডার থেকে রান হলে
      Paths.get(userDir, "src", "main", "scala", "zio", "schema", "codec", "ThriftCodec.scala"),

      // অপশন ৩: সরাসরি পাথ (ব্যাকআপ)
      Paths.get(userDir, "scala", "zio", "schema", "codec", "ThriftCodec.scala")
    )

    // প্রিন্ট করে দেখাবে কোথায় কোথায় খুঁজছে (ডিবাগিংয়ের জন্য)
    println(s"\n[DEBUG] Searching for ThriftCodec.scala in:")
    possiblePaths.foreach(p => println(s" - $p"))

    // ফাইল খোঁজা শুরু
    possiblePaths.find(p => Files.exists(p)) match {
      case Some(path) => 
        println(s"[SUCCESS] Found file at: $path\n")
        val source = Source.fromFile(path.toFile)
        try source.mkString finally source.close()
      case None =>
        throw new RuntimeException(s"CRITICAL: Could not find ThriftCodec.scala anywhere! Checked in: ${userDir}")
    }
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Migration Verification")(

    test("Verify ThriftCodec.scala is migrated to ZIO 2") {
      for {
        content <- findThriftCodecFile()
        _       <- ZIO.succeed("File read successfully. Analyzing code...") 
      } yield {
        // প্রমাণ ১: ZIO 1 এর 'ZTransducer' থাকা যাবে না
        val noZTransducer = !content.contains("ZTransducer")
        
        // প্রমাণ ২: ZIO 1 এর 'unsafeRun' থাকা যাবে না
        val noUnsafeRun   = !content.contains("unsafeRun(")

        // প্রমাণ ৩: ZIO 2 এর 'ZPipeline' থাকতে হবে
        val hasZPipeline  = content.contains("ZPipeline")
        
        // প্রমাণ ৪: ZIO 2 এর 'Unsafe.unsafe' ব্লক থাকতে হবে
        val hasUnsafeBlock = content.contains("Unsafe.unsafe")

        // সব শর্ত পূরণ হলেই পাস
        assertTrue(noZTransducer, noUnsafeRun, hasZPipeline, hasUnsafeBlock)
      }
    }
  )
}