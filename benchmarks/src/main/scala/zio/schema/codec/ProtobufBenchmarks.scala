package zio.schema.codec

import org.openjdk.jmh.annotations._
import zio.internal.{Platform, Tracing}
import zio.BootstrapRuntime

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 10, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(1)
class ProtobufBenchmarks {
  import CodecBenchmarks._

  val TracedRuntime: BootstrapRuntime = new BootstrapRuntime {
    override val platform = Platform.benchmark.withTracing(Tracing.enabled)
  }

  @Param(Array("1000"))
  var size: Int = _

  var outputs: Array[Any] = _

  @Setup
  def allocateOutputs(): Unit = {
    outputs = Array.ofDim[Any](size)
  }

  @Benchmark
  def enumEncoding(): Array[Any] = {
    for (i <- 0 until size) {
      outputs(i) = statusProtobufEncoder(statuses(i % 3))
    }
    outputs
  }

  @Benchmark
  def enumDecoding(): Array[Any] = {
    for (i <- 0 until size) {
      outputs(i) = statusProtobufDecoder(encodedStatuses(i % 3)).getOrElse(throw new RuntimeException)
    }
    outputs
  }

}
