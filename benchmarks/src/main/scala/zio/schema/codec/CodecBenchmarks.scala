//package zio.schema.codec
//
//import zio.{BootstrapRuntime, Chunk}
//import zio.internal.{Platform, Tracing}
//import zio.schema.{DeriveSchema, Schema}
//
//object CodecBenchmarks {
//
//  val TracedRuntime: BootstrapRuntime = new BootstrapRuntime {
//    override val platform = Platform.benchmark.withTracing(Tracing.enabled)
//  }
//
//  sealed trait Status
//  case class Ok(response: List[String]) extends Status
//  case class Failed(code: Int, reason: String, additionalExplanation: Option[String])
//    extends Status
//  case object Pending extends Status
//
//  val statusSchema: Schema[Status] = DeriveSchema.gen[Status]
//  val statusProtobufEncoder: Status => Chunk[Byte] = ProtobufCodec.encode(statusSchema)
//  val statusProtobufDecoder: Chunk[Byte] => Either[String,Status] = ProtobufCodec.decode(statusSchema)
//
//  val statuses: Array[Status] = Array(
//    Ok(List.fill(20)("value")),
//    Failed(99, "reason", Some("explanation")),
//    Ok(List.fill(20)("value2"))
//  )
//
//  val encodedStatuses: Array[Chunk[Byte]] = statuses.map(statusProtobufEncoder(_))
//
//}
