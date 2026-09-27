(ns clj-xet.lz4
  (:import
   (java.nio ByteBuffer ByteOrder)
   (net.jpountz.lz4 LZ4Factory LZ4Compressor LZ4SafeDecompressor)))

(set! *warn-on-reflection* true)

(def ^:private ^LZ4Factory factory (LZ4Factory/fastestInstance))
(def ^:private ^LZ4Compressor compressor (.fastCompressor factory))
(def ^:private ^LZ4SafeDecompressor decompressor (.safeDecompressor factory))

(def ^:private ^:const magic-number 0x184D2204)

(defn write-frame!
  "Compresses the remaining data from the `src` ByteBuffer and writes it as an 
   interoperable LZ4 frame into the `dest` ByteBuffer. Returns `dest`.
   Assumes both buffers use standard endianness (LITTLE_ENDIAN is applied internally)."
  [^ByteBuffer src ^ByteBuffer dest]
  (let [original-order (.order dest)
        src-pos        (.position src)
        src-rem        (.remaining src)]
    (try
      (.order dest ByteOrder/LITTLE_ENDIAN)
      (.putInt dest magic-number)
      ;; frame descriptor
      (.put dest (byte 0x40)) ;; FLG: 0x40 (Block Independence)
      (.put dest (byte 0x40)) ;; BD:  0x40 (64KB Max Block Size)
      (.put dest (byte 0x1D)) ;; HC:  0x1D (Precompiled Header Checksum for 0x40 0x40) 
      (let [block-len-idx (.position dest)]
        (.putInt dest 0) ;; len placeholder
        (let [data-start-idx (.position dest)
              compressed-len (.compress compressor src src-pos src-rem dest data-start-idx (.remaining dest))]
          (.position dest (+ data-start-idx compressed-len))
          (.putInt dest block-len-idx compressed-len)) ;; backfill len
        (.putInt dest 0x00000000) ;; end of frame mark
        (.position src (+ src-pos src-rem))
        dest)
      (finally
        (.order dest original-order)))))

(defn read-frame!
  "Decompresses an interoperable single-block LZ4 frame from the `src` ByteBuffer 
   into the `dest` ByteBuffer. Returns `dest`."
  [^ByteBuffer src ^ByteBuffer dest]
  (let [original-order (.order src)
        src-pos        (.position src)]
    (try
      (.order src ByteOrder/LITTLE_ENDIAN)
      (let [read-magic (.getInt src)]
        (when-not (= read-magic magic-number)
          (throw (IllegalArgumentException.
                  (str "Invalid LZ4 frame magic number: " (Long/toHexString read-magic))))))
      ;; frame descriptor 
      (.get src) ;; FLG
      (.get src) ;; BD
      (.get src) ;; HC
      (let [block-len (.getInt src)
            current-src-idx (.position src)
            dest-pos (.position dest)]
        (let [decompressed-len (.decompress decompressor
                                            src
                                            current-src-idx
                                            block-len
                                            dest
                                            dest-pos
                                            (.remaining dest))]
          (.position dest (+ dest-pos decompressed-len)))
        (.position src (+ current-src-idx block-len 4)) ;; len + end of frame mark
        dest)
      (catch Exception e
        ;; Reset the read pointer on decoding failures to allow retry bounds checking
        (.position src src-pos)
        (throw e))
      (finally
        (.order src original-order)))))
