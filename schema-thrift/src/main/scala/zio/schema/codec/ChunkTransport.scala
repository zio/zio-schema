package zio.schema.codec

import scala.util.control.NoStackTrace

import org.apache.thrift.TConfiguration
import org.apache.thrift.transport.{ TMemoryBuffer, TMemoryInputTransport, TTransport }

import zio.Chunk

object ChunkTransport {

  final private class UnsupportedTransportOperation(msg: String)
      extends UnsupportedOperationException(msg)
      with NoStackTrace

  /**
   * Optimized for Writing
   * TMemoryBuffer
   */
  final class Write(config: TConfiguration = new TConfiguration()) extends TTransport {
    private[this] val buffer = new TMemoryBuffer(1024)

    override def isOpen: Boolean = true
    override def open(): Unit    = ()
    override def close(): Unit   = ()

    override def read(buf: Array[Byte], off: Int, len: Int): Int =
      throw new UnsupportedTransportOperation("ChunkTransport.Write does not support reading")

    override def write(buf: Array[Byte], off: Int, len: Int): Unit =
      buffer.write(buf, off, len)

    override def getConfiguration: TConfiguration = config

    override def updateKnownMessageSize(size: Long): Unit = ()

    override def checkReadBytesAvailable(numBytes: Long): Unit = ()

    def chunk: Chunk[Byte] = Chunk.fromArray(buffer.getArray).take(buffer.length())
  }

  /**
   * Optimized for Reading
   * TMemoryInputTransport
   */
  final class Read(input: Chunk[Byte], config: TConfiguration = new TConfiguration()) extends TTransport {

    private[this] val underlying = new TMemoryInputTransport(config, input.toArray)

    override def isOpen: Boolean = underlying.isOpen
    override def open(): Unit    = underlying.open()
    override def close(): Unit   = underlying.close()

    override def read(buf: Array[Byte], off: Int, len: Int): Int =
      underlying.read(buf, off, len)

    override def write(buf: Array[Byte], off: Int, len: Int): Unit =
      throw new UnsupportedTransportOperation("ChunkTransport.Read does not support writing")

    override def getConfiguration: TConfiguration = config

    override def updateKnownMessageSize(size: Long): Unit =
      underlying.updateKnownMessageSize(size)

    override def checkReadBytesAvailable(numBytes: Long): Unit =
      underlying.checkReadBytesAvailable(numBytes)
  }
}
