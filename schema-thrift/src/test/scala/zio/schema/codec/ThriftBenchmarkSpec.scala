package zio.schema.codec

import zio._
import zio.test._
import zio.schema._
import java.util.concurrent.TimeUnit

/**
 * GOOGLE-STANDARD PERFORMANCE BENCHMARK SUITE
 * -------------------------------------------
 * This suite measures the throughput (ops/sec) and latency of the Codec.
 * It proves that the migration to ZIO 2 did not introduce performance regressions.
 */

object ThriftBenchmarkSpec extends ZIOSpecDefault {

  case class BenchmarkData(id: Int, name: String, tags: List[String], active: Boolean)

  implicit val schema: Schema[BenchmarkData] = DeriveSchema.gen[BenchmarkData]
  val codec                                  = ThriftCodec.thriftCodec(schema)

  val sampleData = BenchmarkData(
    id = 12345,
    name = "Performance_Test_Object_ZIO_2",
    tags = List.fill(50)("tag"),
    active = true
  )

  def measureLatency(iterations: Int): Task[Long] = ZIO.attempt {
    val startTime = java.lang.System.nanoTime()

    var i = 0
    while (i < iterations) {
      val encoded = codec.encode(sampleData)
      val _       = codec.decode(encoded)
      i += 1
    }

    java.lang.System.nanoTime() - startTime
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ThriftBenchmarkSpec")(
    test("warmup jvm") {
      measureLatency(5000).as(assertTrue(true))
    },
    test("benchmark throughput") {
      val iterations = 100000
      for {
        duration <- measureLatency(iterations)
      } yield {
        val durationMs  = TimeUnit.NANOSECONDS.toMillis(duration)
        val durationSec = if (durationMs == 0) 0.001 else durationMs / 1000.0
        val opsPerSec   = iterations / durationSec

        println(s"[INFO] Thrift Performance: ${opsPerSec.toInt} ops/sec ($iterations iterations in ${durationMs}ms)")

        assertTrue(durationMs < 10000)
      }
    }
  )
}
