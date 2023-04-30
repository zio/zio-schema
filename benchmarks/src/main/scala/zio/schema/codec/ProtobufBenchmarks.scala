package zio.schema.codec

import org.openjdk.jmh.annotations._
import zio.Chunk

import java.util.concurrent.TimeUnit
import scala.util.Random

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 10, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@Threads(1)
class ProtobufBenchmarks {
  import CodecBenchmarks._

  @Param(Array("1000"))
  var size: Int = _

  @Param(Array("30721"))
  var bigSize: Int = _

  var outputs: Array[Any] = _

  var bigByteChunk: Chunk[Byte] = _
  var encodedBigByteChunk: Chunk[Byte] = _
  var byteChunkCodec: BinaryCodec[Chunk[Byte]] = _

  @Setup
  def setup(): Unit = {
    outputs = Array.ofDim[Any](size)

    byteChunkCodec = ProtobufCodec.protobufCodec[Chunk[Byte]]
    bigByteChunk = Chunk.fromArray(Random.nextBytes(bigSize))
    encodedBigByteChunk = byteChunkCodec.encode(bigByteChunk)
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

  @Benchmark
  def encodeLargeByteChunk(): Chunk[Byte] =
    byteChunkCodec.encode(bigByteChunk)

  @Benchmark
  def decodeLargeByteChunk(): zio.prelude.Validation[DecodeError, Chunk[Byte]] =
    byteChunkCodec.decode(encodedBigByteChunk)
}

object ProtobufBenchmarksProfiling extends App {

  val bigSize = 30721
  val byteChunkCodec = ProtobufCodec.protobufCodec[Chunk[Byte]]
  val bigByteChunk = Chunk.fromArray(Random.nextBytes(bigSize))
  val encodedBigByteChunk = byteChunkCodec.encode(bigByteChunk)

  while(true) {
    val decoded = byteChunkCodec.decode(encodedBigByteChunk)
    println(s"Decoded ${decoded.map(_.length)} bytes")
  }
}