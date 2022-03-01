package zio.schema.codec

import org.apache.thrift.TConfiguration
import org.apache.thrift.transport.{ TMemoryTransport, TTransport }

import zio.Chunk

object ChunkTransport {

  class Write extends TTransport {
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

    def chunk: Chunk[Byte] =
      Chunk.fromArray(underlying.getOutput.toByteArray)
  }

  class Read(input: Chunk[Byte]) extends TTransport {
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
