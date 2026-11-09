package com.github.luben.zstd

import org.scalatest.flatspec.AnyFlatSpec
import java.io._
import java.nio._
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode
import java.nio.file.StandardOpenOption

import scala.io._
import scala.collection.mutable.WrappedArray

class ZstdDictSpec extends AnyFlatSpec {

  def source = Source.fromFile("src/test/resources/xml")(Codec.ISO8859).map{_.toByte}

  def train(legacy: Boolean): Array[Byte] = {
    val src = source.sliding(1024, 1024).take(1024).map(_.toArray)
    val trainer = new ZstdDictTrainer(1024 * 1024, 32 * 1024)
    for (sample <- src) {
      trainer.addSample(sample)
    }
    val dict = trainer.trainSamples(legacy)
    assert(Zstd.getDictIdFromDict(dict) != 0)
    dict
  }

  "Zstd" should "report error when failing to make a dict" in {
    val src = source.sliding(28, 28).take(4).map(_.toArray)
    val trainer = new ZstdDictTrainer(1024 * 1024, 32 * 1024)
    for (sample <- src) {
      trainer.addSample(sample)
    }
    intercept[com.github.luben.zstd.ZstdException] {
      trainer.trainSamples(false)
    }
  }

  it should "reject trainFromBufferDirect sample sizes exceeding the samples buffer" in {
    val samples = ByteBuffer.allocateDirect(1024)
    val dictBuf = ByteBuffer.allocateDirect(1024)
    // total of the sample sizes is past the end of the samples buffer
    assert(Zstd.isError(Zstd.trainFromBufferDirect(samples, Array(512, 512, 4096), dictBuf, false)))
    assert(Zstd.isError(Zstd.trainFromBufferDirect(samples, Array(512, 512, 4096), dictBuf, true)))
    assert(Zstd.isError(Zstd.trainFromBufferDirect(ByteBuffer.allocateDirect(16), Array(100, 100), dictBuf, true)))
    // sum of the sample sizes wraps around a 32-bit size_t
    assert(Zstd.isError(Zstd.trainFromBufferDirect(samples, Array(Int.MaxValue, Int.MaxValue, 2), dictBuf, true)))
    // negative sample size
    assert(Zstd.isError(Zstd.trainFromBufferDirect(samples, Array(512, -1, 512), dictBuf, false)))
    assert(Zstd.isError(Zstd.trainFromBufferDirect(samples, Array(-1), dictBuf, true)))
  }

  // (offset, length) pairs that point outside of a 16 bytes dictionary buffer
  val invalidDictRanges = List(
    (0, 4096),              // length past the end of the buffer
    (0, 50000000),          // length far past the end of the buffer
    (8, 9),                 // offset + length past the end of the buffer
    (8, Int.MaxValue),      // offset + length overflows
    (-1, 16),               // negative offset
    (-16, 8),               // negative offset
    (Int.MinValue, 16),     // negative offset, dict.length - offset overflows
    (0, -1),                // negative length
    (17, 0)                 // offset past the end of the buffer
  )

  for ((offset, length) <- invalidDictRanges) {
    it should s"reject ZstdDictCompress with out of bounds offset $offset and length $length" in {
      val dictBuff = new Array[Byte](16)
      intercept[IllegalArgumentException] {
        new ZstdDictCompress(dictBuff, offset, length, 1)
      }
    }

    it should s"reject ZstdDictDecompress with out of bounds offset $offset and length $length" in {
      val dictBuff = new Array[Byte](16)
      intercept[IllegalArgumentException] {
        new ZstdDictDecompress(dictBuff, offset, length)
      }
    }
  }

  val input = source.toArray
  val legacyS = List(true, false)
  val levels = List(1)
  for {
       legacy <- legacyS
       dict = train(legacy)
       level <- levels
  } {

    "Zstd" should s"round-trip compression/decompression with dict at level $level with legacy $legacy" in {
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val decompressed = Zstd.decompress(compressed, dict, input.length)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression ByteBuffers with dict at level $level with legacy $legacy" in {
      val size = input.length
      val inBuf = ByteBuffer.allocateDirect(size)
      inBuf.put(input)
      inBuf.flip()
      val compressed = ByteBuffer.allocateDirect(Zstd.compressBound(size).toInt);
      Zstd.compress(compressed, inBuf, dict, level)
      compressed.flip()
      val decompressed = ByteBuffer.allocateDirect(size)
      Zstd.decompress(decompressed, compressed, dict)
      decompressed.flip()
      val out = new Array[Byte](decompressed.remaining)
      decompressed.get(out)
      assert(input.toSeq == out.toSeq)
    }

    it should s"round-trip compression/decompression with fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, level)
      val compressed = Zstd.compress(input, cdict)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression with fast dict at offset at level $level with legacy $legacy" in {
      val size = input.length
      val offset = 7
      val padded = Array.fill[Byte](offset)(42) ++ dict ++ Array.fill[Byte](13)(42)
      val cdict = new ZstdDictCompress(padded, offset, dict.length, level)
      val compressed = Zstd.compress(input, cdict)
      cdict.close
      val ddict = new ZstdDictDecompress(padded, offset, dict.length)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression in place with fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, level)
      val compressed = new Array[Byte](size)
      val compressed_size = Zstd.compress(compressed, input, cdict)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = new Array[Byte](size)
      Zstd.decompressFastDict(decompressed, 0, compressed, 0, compressed_size.toInt, ddict)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression ByteBuffers with fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val inBuf = ByteBuffer.allocateDirect(size)
      inBuf.put(input)
      inBuf.flip()
      val cdict = new ZstdDictCompress(dict, level)
      val compressed = ByteBuffer.allocateDirect(Zstd.compressBound(size).toInt);
      Zstd.compress(compressed, inBuf, cdict)
      compressed.flip()
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = ByteBuffer.allocateDirect(size)
      Zstd.decompress(decompressed, compressed, ddict)
      decompressed.flip()
      ddict.close
      val out = new Array[Byte](decompressed.remaining)
      decompressed.get(out)
      assert(Zstd.getDictIdFromFrameBuffer(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == out.toSeq)
    }

    it should s"round-trip compression/decompression with byte[]/fast dict at level $level with legacy $legacy" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip compression/decompression with fast/byte[] dict at level $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val compressed = Zstd.compress(input, cdict)
      cdict.close
      val decompressed = Zstd.decompress(compressed, dict, size)
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a byte[] and decompress with a ByteBuffer using byte[] dict $level with legacy $legacy" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, dict, level)
      val compressedBuffer = ByteBuffer.allocateDirect(compressed.size)
      compressedBuffer.put(compressed)
      compressedBuffer.limit(compressedBuffer.position())
      compressedBuffer.flip()

      val decompressedBuffer = Zstd.decompress(compressedBuffer, dict, size)
      val decompressed = new Array[Byte](size)
      decompressedBuffer.get(decompressed)
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a byte[] and decompress with a ByteBuffer using fast dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val compressed = Zstd.compress(input, cdict)
      val compressedBuffer = ByteBuffer.allocateDirect(compressed.size)
      compressedBuffer.put(compressed)
      compressedBuffer.flip()
      cdict.close

      val ddict = new ZstdDictDecompress(dict)
      val decompressedBuffer = Zstd.decompress(compressedBuffer, ddict, size)
      val decompressed = new Array[Byte](size)
      decompressedBuffer.get(decompressed)
      ddict.close
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a ByteBuffer and decompress with a byte[] using fast dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val inputBuffer = ByteBuffer.allocateDirect(size)
      inputBuffer.put(input)
      inputBuffer.flip()
      val compressedBuffer: ByteBuffer = Zstd.compress(inputBuffer, cdict)
      val compressed = new Array[Byte](compressedBuffer.limit() - compressedBuffer.position())
      compressedBuffer.get(compressed)
      cdict.close
      val ddict = new ZstdDictDecompress(dict)
      val decompressed = Zstd.decompress(compressed, ddict, size)
      ddict.close
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"compress with a ByteBuffer and decompress with a byte[] using byte[] dict $level with legacy $legacy" in {
      val size = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, level)
      val inputBuffer = ByteBuffer.allocateDirect(size)
      inputBuffer.put(input)
      inputBuffer.flip()
      val compressedBuffer: ByteBuffer = Zstd.compress(inputBuffer, dict, level)
      val compressed = new Array[Byte](compressedBuffer.limit() - compressedBuffer.position())
      compressedBuffer.get(compressed)
      cdict.close
      val decompressed = Zstd.decompress(compressed, dict, size)
      assert(Zstd.getDictIdFromFrameBuffer(compressedBuffer) == Zstd.getDictIdFromDict(dict))
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == decompressed.toSeq)
    }

    it should s"round-trip streaming compression/decompression with byte[] dict with legacy $legacy " in {
      val size  = input.length
      val os    = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
      val zos   = new ZstdOutputStream(os, 1)
      zos.setDict(dict)
      val block = 128 * 1024
      var ptr   = 0
      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zos.write(input, ptr, chunk)
        ptr += chunk
      }
      zos.flush
      zos.close
      val compressed = os.toByteArray
      // now decompress
      val is    = new ByteArrayInputStream(compressed)
      val zis   = new ZstdInputStream(is)
      zis.setDict(dict)
      val output= Array.fill[Byte](size)(0)
      ptr       = 0

      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zis.read(output, ptr, chunk)
        ptr += chunk
      }
      zis.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming compression/decompression with fast dict with legacy $legacy " in {
      val size  = input.length
      val cdict = new ZstdDictCompress(dict, 0, dict.size, 1)
      val os    = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
      val zos   = new ZstdOutputStream(os, 1)
      zos.setDict(cdict)
      val block = 128 * 1024
      var ptr   = 0
      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zos.write(input, ptr, chunk)
        ptr += chunk
      }
      zos.close
      val compressed = os.toByteArray

      // now decompress
      val ddict = new ZstdDictDecompress(dict)
      val is    = new ByteArrayInputStream(compressed)
      val zis   = new ZstdInputStream(is)
      zis.setDict(ddict)
      val output= Array.fill[Byte](size)(0)
      ptr       = 0

      while (ptr < size) {
        val chunk = if (size - ptr > block) block else size - ptr
        zis.read(output, ptr, chunk)
        ptr += chunk
      }
      zis.close
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(dict))
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming ByteBuffer compression/decompression with byte[] dict with legacy $legacy" in {
      val size  = input.length
      val os    = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)
      // compress
      val ib    = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStream(os, 1)
      osw.setDict(dict)
      osw.compress(ib)
      osw.flush
      osw.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict))

      // now decompress
      val zis   = new ZstdDirectBufferDecompressingStream(os)
      zis.setDict(dict)
      val output= Array.fill[Byte](size)(0)
      val block = ByteBuffer.allocateDirect(128 * 1024)
      var offset = 0
      while (zis.hasRemaining) {
        block.clear()
        val read = zis.read(block)
        block.flip()
        block.get(output, offset, read)
        offset += read
      }
      zis.close
      assert(input.toSeq == output.toSeq)
    }

    it should s"round-trip streaming ByteBuffer compression/decompression with fast dict with legacy $legacy" in {
      val cdict = new ZstdDictCompress(dict, 0, dict.size, 1)
      val size  = input.length
      val os    = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)

      // compress
      val ib    = ByteBuffer.allocateDirect(size)
      ib.put(input)
      ib.flip

      val osw = new ZstdDirectBufferCompressingStream(os, 1)
      osw.setDict(cdict)
      osw.compress(ib)
      osw.close
      os.flip

      assert(Zstd.getDictIdFromFrameBuffer(os) == Zstd.getDictIdFromDict(dict))

      // now decompress
      val zis   = new ZstdDirectBufferDecompressingStream(os)
      val ddict = new ZstdDictDecompress(dict)
      zis.setDict(ddict)
      val output= Array.fill[Byte](size)(0)
      val block = ByteBuffer.allocateDirect(128 * 1024)
      var offset = 0
      while (zis.hasRemaining) {
        block.clear()
        val read = zis.read(block)
        block.flip()
        block.get(output, offset, read)
        offset += read
      }
      zis.close
      assert(input.toSeq == output.toSeq)
    }
  }

  // The native streams keep referencing the fast dicts set on them, so a dict
  // must stay locked (it can not be closed and freed) while a stream uses it.
  lazy val streamDict = train(false)

  def readFully(zis: InputStream, size: Int): Array[Byte] = {
    val output = Array.fill[Byte](size)(0)
    var ptr = 0
    while (ptr < size) {
      val read = zis.read(output, ptr, size - ptr)
      assert(read > 0)
      ptr += read
    }
    output
  }

  for (finalizer <- List(true, false)) {
    def newInputStream(is: InputStream, ddict: ZstdDictDecompress): InputStream =
      if (finalizer) new ZstdInputStream(is).setDict(ddict)
      else new ZstdInputStreamNoFinalizer(is).setDict(ddict)

    def newOutputStream(os: OutputStream, cdict: ZstdDictCompress): OutputStream =
      if (finalizer) new ZstdOutputStream(os, 1).setDict(cdict)
      else new ZstdOutputStreamNoFinalizer(os, 1).setDict(cdict)

    "ZstdInputStream" should s"keep the fast dict locked while in use by the stream with finalizer $finalizer" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, streamDict, 1)
      val ddict = new ZstdDictDecompress(streamDict)
      val zis = newInputStream(new ByteArrayInputStream(compressed), ddict)
      // closing the dict while the stream references it must not free it
      intercept[IllegalStateException] {
        ddict.close
      }
      // the stream can still use the dict
      val output = readFully(zis, size)
      assert(input.toSeq == output.toSeq)
      zis.close
      // closing the stream releases the dict
      ddict.close
    }

    "ZstdOutputStream" should s"keep the fast dict locked while in use by the stream with finalizer $finalizer" in {
      val size = input.length
      val cdict = new ZstdDictCompress(streamDict, 1)
      val os = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
      val zos = newOutputStream(os, cdict)
      // closing the dict while the stream references it must not free it
      intercept[IllegalStateException] {
        cdict.close
      }
      // the stream can still use the dict
      zos.write(input)
      zos.close
      // closing the stream releases the dict
      cdict.close
      val compressed = os.toByteArray
      assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(streamDict))
      val decompressed = Zstd.decompress(compressed, streamDict, size)
      assert(input.toSeq == decompressed.toSeq)
    }
  }

  "ZstdInputStreamNoFinalizer" should "release the lock of the previous fast dict when setting a new one" in {
    val size = input.length
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict1 = new ZstdDictDecompress(streamDict)
    val ddict2 = new ZstdDictDecompress(streamDict)
    val zis = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(compressed))
    zis.setDict(ddict1)
    zis.setDict(ddict2)
    // the stream does not reference the first dict anymore
    ddict1.close
    intercept[IllegalStateException] {
      ddict2.close
    }
    val output = readFully(zis, size)
    assert(input.toSeq == output.toSeq)
    zis.close
    ddict2.close
  }

  it should "keep the current fast dict locked when setting a new one fails" in {
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict1 = new ZstdDictDecompress(streamDict)
    val ddict2 = new ZstdDictDecompress(streamDict)
    val zis = new ZstdInputStreamNoFinalizer(new ByteArrayInputStream(compressed))
    zis.setDict(ddict1)
    // start decompressing the frame, the dict can't be changed in the middle of it
    assert(zis.read() == (input(0) & 0xff))
    intercept[ZstdIOException] {
      zis.setDict(ddict2)
    }
    // the failed call did not keep the new dict locked
    ddict2.close
    // the stream still references the first dict
    intercept[IllegalStateException] {
      ddict1.close
    }
    zis.close
    ddict1.close
  }

  "ZstdOutputStreamNoFinalizer" should "release the lock of the previous fast dict when setting a new one" in {
    val size = input.length
    val cdict1 = new ZstdDictCompress(streamDict, 1)
    val cdict2 = new ZstdDictCompress(streamDict, 1)
    val os = new ByteArrayOutputStream(Zstd.compressBound(size.toLong).toInt)
    val zos = new ZstdOutputStreamNoFinalizer(os, 1)
    zos.setDict(cdict1)
    zos.setDict(cdict2)
    // the stream does not reference the first dict anymore
    cdict1.close
    intercept[IllegalStateException] {
      cdict2.close
    }
    zos.write(input)
    zos.close
    cdict2.close
    val decompressed = Zstd.decompress(os.toByteArray, streamDict, size)
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "keep the current fast dict locked when setting a new one fails" in {
    val cdict1 = new ZstdDictCompress(streamDict, 1)
    val cdict2 = new ZstdDictCompress(streamDict, 1)
    val os = new ByteArrayOutputStream(Zstd.compressBound(input.length.toLong).toInt)
    val zos = new ZstdOutputStreamNoFinalizer(os, 1)
    zos.setDict(cdict1)
    // start a frame, the dict can't be changed in the middle of it
    zos.write(input, 0, 1024)
    intercept[IllegalStateException] {
      zos.setDict(cdict2)
    }
    // the failed call did not keep the new dict locked
    cdict2.close
    // the stream still references the first dict
    intercept[IllegalStateException] {
      cdict1.close
    }
    zos.close
    cdict1.close
  }

  // The native compression/decompression contexts keep referencing the fast dicts
  // loaded in them, so a dict must stay locked (it can not be closed and freed)
  // while a context uses it.
  "ZstdCompressCtx" should "keep the fast dict locked while it is loaded" in {
    val size = input.length
    val cdict = new ZstdDictCompress(streamDict, 1)
    val cctx = new ZstdCompressCtx()
    cctx.loadDict(cdict)
    // closing the dict while the context references it must not free it
    intercept[IllegalStateException] {
      cdict.close
    }
    // the context can still use the dict
    val compressed = cctx.compress(input)
    cctx.close
    // closing the context releases the dict
    cdict.close
    assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(streamDict))
    val decompressed = Zstd.decompress(compressed, streamDict, size)
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "release the lock of the previous fast dict when loading a new one" in {
    val size = input.length
    val cdict1 = new ZstdDictCompress(streamDict, 1)
    val cdict2 = new ZstdDictCompress(streamDict, 1)
    val cctx = new ZstdCompressCtx()
    cctx.loadDict(cdict1)
    cctx.loadDict(cdict2)
    // the context does not reference the first dict anymore
    cdict1.close
    intercept[IllegalStateException] {
      cdict2.close
    }
    val compressed = cctx.compress(input)
    cctx.close
    cdict2.close
    val decompressed = Zstd.decompress(compressed, streamDict, size)
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "release the lock of the fast dict when loading a byte[] dict" in {
    val size = input.length
    val cdict = new ZstdDictCompress(streamDict, 1)
    val cctx = new ZstdCompressCtx()
    cctx.loadDict(cdict)
    cctx.loadDict(streamDict)
    // the context does not reference the fast dict anymore
    cdict.close
    val compressed = cctx.compress(input)
    cctx.close
    assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(streamDict))
    val decompressed = Zstd.decompress(compressed, streamDict, size)
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "release the lock of the fast dict when removing it" in {
    val size = input.length
    val cdict = new ZstdDictCompress(streamDict, 1)
    val cctx = new ZstdCompressCtx()
    cctx.loadDict(cdict)
    cctx.loadDict(null.asInstanceOf[ZstdDictCompress])
    // the context does not reference the fast dict anymore
    cdict.close
    val compressed = cctx.compress(input)
    cctx.close
    assert(Zstd.getDictIdFromFrame(compressed) == 0)
    assert(input.toSeq == Zstd.decompress(compressed, size).toSeq)
  }

  it should "release the lock of the fast dict when the context is reset" in {
    val size = input.length
    val cdict = new ZstdDictCompress(streamDict, 1)
    val cctx = new ZstdCompressCtx()
    cctx.loadDict(cdict)
    cctx.reset()
    // the context does not reference the fast dict anymore
    cdict.close
    val compressed = cctx.compress(input)
    cctx.close
    assert(Zstd.getDictIdFromFrame(compressed) == 0)
    assert(input.toSeq == Zstd.decompress(compressed, size).toSeq)
  }

  it should "keep the current fast dict locked when loading a new one fails" in {
    val cdict1 = new ZstdDictCompress(streamDict, 1)
    val cdict2 = new ZstdDictCompress(streamDict, 1)
    val cctx = new ZstdCompressCtx()
    cctx.loadDict(cdict1)
    // start a frame, the dict can't be changed in the middle of it
    val src = ByteBuffer.allocateDirect(1024)
    src.put(input, 0, 1024)
    src.flip()
    val dst = ByteBuffer.allocateDirect(Zstd.compressBound(1024).toInt)
    cctx.compressDirectByteBufferStream(dst, src, EndDirective.CONTINUE)
    intercept[ZstdException] {
      cctx.loadDict(cdict2)
    }
    // the failed call did not keep the new dict locked
    cdict2.close
    // the context still references the first dict
    intercept[IllegalStateException] {
      cdict1.close
    }
    cctx.close
    cdict1.close
  }

  "ZstdDecompressCtx" should "keep the fast dict locked while it is loaded" in {
    val size = input.length
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict = new ZstdDictDecompress(streamDict)
    val dctx = new ZstdDecompressCtx()
    dctx.loadDict(ddict)
    // closing the dict while the context references it must not free it
    intercept[IllegalStateException] {
      ddict.close
    }
    // the context can still use the dict
    val decompressed = dctx.decompress(compressed, size)
    dctx.close
    // closing the context releases the dict
    ddict.close
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "release the lock of the previous fast dict when loading a new one" in {
    val size = input.length
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict1 = new ZstdDictDecompress(streamDict)
    val ddict2 = new ZstdDictDecompress(streamDict)
    val dctx = new ZstdDecompressCtx()
    dctx.loadDict(ddict1)
    dctx.loadDict(ddict2)
    // the context does not reference the first dict anymore
    ddict1.close
    intercept[IllegalStateException] {
      ddict2.close
    }
    val decompressed = dctx.decompress(compressed, size)
    dctx.close
    ddict2.close
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "release the lock of the fast dict when loading a byte[] dict" in {
    val size = input.length
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict = new ZstdDictDecompress(streamDict)
    val dctx = new ZstdDecompressCtx()
    dctx.loadDict(ddict)
    dctx.loadDict(streamDict)
    // the context does not reference the fast dict anymore
    ddict.close
    val decompressed = dctx.decompress(compressed, size)
    dctx.close
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "release the lock of the fast dict when removing it" in {
    val size = input.length
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict = new ZstdDictDecompress(streamDict)
    val dctx = new ZstdDecompressCtx()
    dctx.loadDict(ddict)
    dctx.loadDict(null.asInstanceOf[ZstdDictDecompress])
    // the context does not reference the fast dict anymore
    ddict.close
    intercept[ZstdException] {
      dctx.decompress(compressed, size)
    }
    dctx.close
  }

  it should "release the lock of the fast dict when the context is reset" in {
    val size = input.length
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict = new ZstdDictDecompress(streamDict)
    val dctx = new ZstdDecompressCtx()
    dctx.loadDict(ddict)
    dctx.reset()
    // the context does not reference the fast dict anymore
    ddict.close
    intercept[ZstdException] {
      dctx.decompress(compressed, size)
    }
    dctx.close
  }

  it should "keep the current fast dict locked when loading a new one fails" in {
    val compressed = Zstd.compressUsingDict(input, streamDict, 1)
    val ddict1 = new ZstdDictDecompress(streamDict)
    val ddict2 = new ZstdDictDecompress(streamDict)
    val dctx = new ZstdDecompressCtx()
    dctx.loadDict(ddict1)
    // start decompressing a frame, the dict can't be changed in the middle of it
    val src = ByteBuffer.allocateDirect(compressed.length)
    src.put(compressed, 0, 4)
    src.flip()
    val dst = ByteBuffer.allocateDirect(input.length)
    dctx.decompressDirectByteBufferStream(dst, src)
    intercept[ZstdException] {
      dctx.loadDict(ddict2)
    }
    // the failed call did not keep the new dict locked
    ddict2.close
    // the context still references the first dict
    intercept[IllegalStateException] {
      ddict1.close
    }
    dctx.close
    ddict1.close
  }

  "ZstdDirectBufferCompressingStream" should "keep the fast dict locked while in use by the stream" in {
    val size = input.length
    val cdict = new ZstdDictCompress(streamDict, 1)
    val target = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)
    val zos = new ZstdDirectBufferCompressingStream(target, 1)
    zos.setDict(cdict)
    // closing the dict before the stream starts using it must not free it
    intercept[IllegalStateException] {
      cdict.close
    }
    val src = ByteBuffer.allocateDirect(size)
    src.put(input)
    src.flip()
    src.limit(size / 2)
    zos.compress(src)
    // closing the dict in the middle of the frame must not free it
    intercept[IllegalStateException] {
      cdict.close
    }
    // the stream can still use the dict
    src.limit(size)
    zos.compress(src)
    zos.close
    // closing the stream releases the dict
    cdict.close
    target.flip()
    val compressed = new Array[Byte](target.remaining)
    target.get(compressed)
    assert(Zstd.getDictIdFromFrame(compressed) == Zstd.getDictIdFromDict(streamDict))
    val decompressed = Zstd.decompress(compressed, streamDict, size)
    assert(input.toSeq == decompressed.toSeq)
  }

  "ZstdDirectBufferCompressingStreamNoFinalizer" should "release the lock of the previous fast dict when setting a new dict" in {
    val size = input.length
    val cdict1 = new ZstdDictCompress(streamDict, 1)
    val cdict2 = new ZstdDictCompress(streamDict, 1)
    val cdict3 = new ZstdDictCompress(streamDict, 1)
    val target = ByteBuffer.allocateDirect(Zstd.compressBound(size.toLong).toInt)
    val zos = new ZstdDirectBufferCompressingStreamNoFinalizer(target, 1)
    zos.setDict(cdict1)
    zos.setDict(cdict2)
    // the stream does not reference the first dict anymore
    cdict1.close
    intercept[IllegalStateException] {
      cdict2.close
    }
    zos.setDict(streamDict)
    // the stream does not reference the second dict anymore
    cdict2.close
    zos.setDict(cdict3)
    val src = ByteBuffer.allocateDirect(size)
    src.put(input)
    src.flip()
    zos.compress(src)
    intercept[IllegalStateException] {
      cdict3.close
    }
    zos.close
    cdict3.close
    target.flip()
    val compressed = new Array[Byte](target.remaining)
    target.get(compressed)
    val decompressed = Zstd.decompress(compressed, streamDict, size)
    assert(input.toSeq == decompressed.toSeq)
  }

  it should "not keep the fast dict locked when set on a closed stream" in {
    val zos = new ZstdDirectBufferCompressingStreamNoFinalizer(ByteBuffer.allocateDirect(100), 1)
    zos.close()
    val cdict = new ZstdDictCompress(streamDict, 1)
    intercept[IllegalStateException] {
      zos.setDict(cdict)
    }
    // the dict is not retained by the closed stream
    cdict.close
  }

  def newBufferDecompressingStream(direct: Boolean, compressed: Array[Byte]): BaseZstdBufferDecompressingStreamNoFinalizer =
    if (direct) {
      val source = ByteBuffer.allocateDirect(compressed.length)
      source.put(compressed)
      source.flip()
      new ZstdDirectBufferDecompressingStreamNoFinalizer(source)
    } else {
      new ZstdBufferDecompressingStreamNoFinalizer(ByteBuffer.wrap(compressed))
    }

  def newTargetBuffer(direct: Boolean, size: Int): ByteBuffer =
    if (direct) ByteBuffer.allocateDirect(size) else ByteBuffer.allocate(size)

  def readBufferStream(zis: BaseZstdBufferDecompressingStreamNoFinalizer, direct: Boolean, size: Int): Array[Byte] = {
    val output = Array.fill[Byte](size)(0)
    val block = newTargetBuffer(direct, 128 * 1024)
    var offset = 0
    while (zis.hasRemaining) {
      block.clear()
      val read = zis.read(block)
      block.flip()
      block.get(output, offset, read)
      offset += read
    }
    assert(offset == size)
    output
  }

  for (direct <- List(true, false)) {
    val streamName =
      if (direct) "ZstdDirectBufferDecompressingStreamNoFinalizer"
      else "ZstdBufferDecompressingStreamNoFinalizer"

    streamName should "keep the fast dict locked while in use by the stream" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, streamDict, 1)
      val ddict = new ZstdDictDecompress(streamDict)
      val zis = newBufferDecompressingStream(direct, compressed)
      zis.setDict(ddict)
      // closing the dict while the stream references it must not free it
      intercept[IllegalStateException] {
        ddict.close
      }
      // the stream can still use the dict
      val output = readBufferStream(zis, direct, size)
      zis.close
      // closing the stream releases the dict
      ddict.close
      assert(input.toSeq == output.toSeq)
    }

    it should "release the lock of the previous fast dict when setting a new one" in {
      val size = input.length
      val compressed = Zstd.compressUsingDict(input, streamDict, 1)
      val ddict1 = new ZstdDictDecompress(streamDict)
      val ddict2 = new ZstdDictDecompress(streamDict)
      val zis = newBufferDecompressingStream(direct, compressed)
      zis.setDict(ddict1)
      zis.setDict(ddict2)
      // the stream does not reference the first dict anymore
      ddict1.close
      intercept[IllegalStateException] {
        ddict2.close
      }
      val output = readBufferStream(zis, direct, size)
      zis.close
      ddict2.close
      assert(input.toSeq == output.toSeq)
    }

    it should "keep the current fast dict locked when setting a new one fails" in {
      val compressed = Zstd.compressUsingDict(input, streamDict, 1)
      val ddict1 = new ZstdDictDecompress(streamDict)
      val ddict2 = new ZstdDictDecompress(streamDict)
      val zis = newBufferDecompressingStream(direct, compressed)
      zis.setDict(ddict1)
      // start decompressing the frame, the dict can't be changed in the middle of it
      zis.read(newTargetBuffer(direct, 1))
      intercept[ZstdIOException] {
        zis.setDict(ddict2)
      }
      // the failed call did not keep the new dict locked
      ddict2.close
      // the stream still references the first dict
      intercept[IllegalStateException] {
        ddict1.close
      }
      zis.close
      ddict1.close
    }

    it should "throw on setting parameters on closed stream" in {
      val compressed = Zstd.compressUsingDict(input, streamDict, 1)
      val zis = newBufferDecompressingStream(direct, compressed)
      zis.close
      assert(!zis.hasRemaining)
      intercept[IOException] {
        zis.setLongMax(20)
      }
      intercept[IOException] {
        zis.setDict(streamDict)
      }
      val ddict = new ZstdDictDecompress(streamDict)
      intercept[IOException] {
        zis.setDict(ddict)
      }
      // the dict is not retained by the closed stream
      ddict.close
    }
  }
}
