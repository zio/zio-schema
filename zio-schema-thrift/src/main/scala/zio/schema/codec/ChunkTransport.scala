package zio.schema.codec

import org.apache.thrift.TConfiguration
import org.apache.thrift.transport.{TMemoryTransport, TTransport}

import zio.Chunk

/** Thrift transports over in-memory bytes, used by [[ThriftCodec]]. */
object ChunkTransport {

  /**
   * A write-only Thrift transport that collects the written bytes in memory;
   * `read` throws `java.lang.UnsupportedOperationException`.
   */
  class Write extends TTransport {

    /** The in-memory transport that holds the written bytes. */
    val underlying = new TMemoryTransport(Array.emptyByteArray)

    override def isOpen: Boolean = underlying.isOpen

    override def open(): Unit = underlying.open()

    override def close(): Unit = underlying.close()

    override def read(buf: Array[Byte], off: Int, len: Int): Int =
      throw new UnsupportedOperationException

    override def write(buf: Array[Byte], off: Int, len: Int): Unit =
      underlying.write(buf, off, len)

    override def getConfiguration: TConfiguration =
      underlying.getConfiguration

    override def updateKnownMessageSize(size: Long): Unit =
      underlying.updateKnownMessageSize(size)

    override def checkReadBytesAvailable(numBytes: Long): Unit =
      underlying.checkReadBytesAvailable(numBytes)

    /** Returns every byte written so far, from the first write on. */
    def chunk: Chunk[Byte] =
      Chunk.fromArray(underlying.getOutput.toByteArray)
  }

  /**
   * A read-only Thrift transport over `input`; `write` throws
   * `java.lang.UnsupportedOperationException`.
   */
  class Read(input: Chunk[Byte]) extends TTransport {

    /** The in-memory transport that reads `input`. */
    val underlying = new TMemoryTransport(input.toArray)

    override def isOpen: Boolean = underlying.isOpen

    override def open(): Unit = underlying.open()

    override def close(): Unit = underlying.close()

    override def read(buf: Array[Byte], off: Int, len: Int): Int =
      underlying.read(buf, off, len)

    override def write(buf: Array[Byte], off: Int, len: Int): Unit =
      throw new UnsupportedOperationException

    override def getConfiguration: TConfiguration =
      underlying.getConfiguration

    override def updateKnownMessageSize(size: Long): Unit =
      underlying.updateKnownMessageSize(size)

    override def checkReadBytesAvailable(numBytes: Long): Unit =
      underlying.checkReadBytesAvailable(numBytes)
  }

}
