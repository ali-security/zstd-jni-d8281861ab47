package com.github.luben.zstd;

import java.io.Closeable;
import java.io.IOException;
import java.nio.ByteBuffer;

public abstract class BaseZstdBufferDecompressingStreamNoFinalizer implements Closeable {
    protected long stream;
    protected ByteBuffer source;
    protected boolean closed = false;
    private boolean finishedFrame = false;
    private boolean streamEnd = false;
    // the dictionary currently referenced by the native stream (if any)
    private ZstdDictDecompress active_dict = null;
    private int consumed;
    private int produced;

    BaseZstdBufferDecompressingStreamNoFinalizer(ByteBuffer source) {
        this.source = source;
    }

    /**
     * Override this method in case the byte buffer passed to the constructor might not contain the full compressed stream
     *
     * @param toRefill current buffer
     * @return either the current buffer (but refilled and flipped if there was new content) or a new buffer.
     */
    protected ByteBuffer refill(ByteBuffer toRefill) {
        return toRefill;
    }

    public boolean hasRemaining() {
        return !closed && !streamEnd && (source.hasRemaining() || !finishedFrame);
    }

    public BaseZstdBufferDecompressingStreamNoFinalizer setDict(byte[] dict) throws IOException {
        if (dict == null) {
            throw new NullPointerException("dict");
        }
        if (closed) {
            throw new IOException("Stream closed");
        }
        long size = Zstd.loadDictDecompress(stream, dict, dict.length);
        if (Zstd.isError(size)) {
            throw new ZstdIOException(size);
        }
        return this;
    }

    public BaseZstdBufferDecompressingStreamNoFinalizer setDict(ZstdDictDecompress dict) throws IOException {
        if (dict == null) {
            throw new NullPointerException("dict");
        }
        if (closed) {
            throw new IOException("Stream closed");
        }
        // the native stream keeps referencing the dict, so hold its shared lock
        // (it can't be closed and freed) for as long as it is in use
        dict.acquireSharedLock();
        long size = Zstd.loadFastDictDecompress(stream, dict);
        if (Zstd.isError(size)) {
            dict.releaseSharedLock();
            throw new ZstdIOException(size);
        }
        // release the shared lock on the previously used dict (if any)
        if (active_dict != null) {
            active_dict.releaseSharedLock();
        }
        // keep the dict alive so it's not garbage collected
        active_dict = dict;
        return this;
    }

    public BaseZstdBufferDecompressingStreamNoFinalizer setLongMax(int windowLogMax) throws IOException {
        if (closed) {
            throw new IOException("Stream closed");
        }
        long size = Zstd.setDecompressionLongMax(stream, windowLogMax);
        if (Zstd.isError(size)) {
            throw new ZstdIOException(size);
        }
        return this;
    }

    int readInternal(ByteBuffer target, boolean isDirectBufferRequired) throws IOException {
        if (closed) {
            throw new IOException("Stream closed");
        }
        if (streamEnd) {
            return 0;
        }

        long remaining = decompressStream(stream, target, target.position(), target.remaining(), source, source.position(), source.remaining());
        if (Zstd.isError(remaining)) {
            throw new ZstdIOException(remaining);
        }

        source.position(source.position() + consumed);
        target.position(target.position() + produced);

        if (!source.hasRemaining()) {
            source = refill(source);
            if (!isDirectBufferRequired && source.isDirect()) {
                throw new IllegalArgumentException("Source buffer should be a non-direct buffer");
            }
            if (isDirectBufferRequired && !source.isDirect()) {
                throw new IllegalArgumentException("Source buffer should be a direct buffer");
            }
        }

        finishedFrame = remaining == 0;
        if (finishedFrame) {
            // nothing left, so at end of the stream
            streamEnd = !source.hasRemaining();
        }

        return produced;
    }

    @Override
    public void close() {
        if (!closed) {
            try {
                freeDStream(stream);
            } finally {
                closed = true;
                source = null; // help GC with realizing the buffer can be released
                // the native stream is freed, release the dict it was referencing (if any)
                if (active_dict != null) {
                    active_dict.releaseSharedLock();
                    active_dict = null;
                }
            }
        }
    }
    public abstract int read(ByteBuffer target) throws IOException;

    abstract long createDStream();

    abstract long freeDStream(long stream);

    abstract long initDStream(long stream);

    abstract long decompressStream(long stream, ByteBuffer dst, int dstOffset, int dstSize, ByteBuffer src, int srcOffset, int srcSize);
}
