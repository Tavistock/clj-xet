(ns clj-xet.lz4
  (:import
   (java.nio ByteBuffer ByteOrder)
   (net.jpountz.lz4 LZ4Factory LZ4Compressor LZ4SafeDecompressor)
   (net.jpountz.xxhash XXHash32 XXHashFactory)))

(set! *warn-on-reflection* true)

(def ^:private ^LZ4Factory factory (LZ4Factory/fastestInstance))
(def ^:private ^LZ4Compressor compressor (.fastCompressor factory))
(def ^:private ^LZ4SafeDecompressor decompressor (.safeDecompressor factory))
(def ^:private ^XXHash32 xxhash32 (.hash32 (XXHashFactory/fastestInstance)))

(def ^:private ^:const magic-number 0x184D2204)
(def ^:private ^:const skippable-magic-base 0x184D2A50)
(def ^:private ^:const uncompressed-block-flag 0x80000000)

;; FLG bits (see https://github.com/lz4/lz4/blob/dev/doc/lz4_Frame_format.md)
(def ^:private ^:const flg-version 0x40) ;; bits 7-6 must be 01
(def ^:private ^:const flg-block-independence 0x20)
(def ^:private ^:const flg-block-checksum 0x10)
(def ^:private ^:const flg-content-size 0x08)
(def ^:private ^:const flg-content-checksum 0x04)
(def ^:private ^:const flg-dict-id 0x01)

(def ^:private code->block-max-size
  {4 65536, 5 262144, 6 1048576, 7 4194304})

(defn- block-max-size-code
  "Returns the smallest LZ4 block maximum size code (BD bits 6-4) able to hold
   `size` bytes."
  ^long [^long size]
  (cond
    (<= size 65536)   4
    (<= size 262144)  5
    (<= size 1048576) 6
    (<= size 4194304) 7
    :else (throw (IllegalArgumentException.
                  (str "LZ4 block size exceeds the 4 MiB maximum: " size)))))

(defn- header-checksum
  "The frame descriptor checksum: the second byte of the xxHash32 (seed 0) of
   the descriptor bytes in `buf` between `start` (inclusive) and `end`
   (exclusive)."
  ^long [^ByteBuffer buf ^long start ^long end]
  (bit-and (unsigned-bit-shift-right
            (.hash xxhash32 buf (int start) (int (- end start)) 0)
            8)
           0xFF))

(defn max-frame-size
  "Returns an upper bound on the number of bytes `write-frame!` may write to
   `dest` for `n` bytes of input using the default options."
  ^long [^long n]
  (+ 7 4 (.maxCompressedLength compressor (int n)) 4))

(defn write-frame!
  "Compresses the remaining data from the `src` ByteBuffer and writes it as an
   interoperable LZ4 frame into the `dest` ByteBuffer. Returns `dest`.

   Options (all optional):
   - `:block-size`          maximum uncompressed block size; one of 65536,
                            262144, 1048576 or 4194304 (default: the smallest
                            that fits the input, or 65536 for empty input).
   - `:block-independence?` whether blocks are independent (default true).
   - `:block-checksum?`     append an xxHash32 checksum after each block (default false).
   - `:content-size?`       write the uncompressed size into the descriptor (default false).
   - `:content-checksum?`   append an xxHash32 checksum of the content (default false).
   - `:dict-id`             dictionary id to record in the descriptor (default nil).

   Assumes both buffers use standard endianness (LITTLE_ENDIAN is applied internally)."
  [^ByteBuffer src ^ByteBuffer dest & {:keys [block-size block-independence?
                                              block-checksum? content-size?
                                              content-checksum? dict-id]}]
  (let [original-order (.order dest)
        src-pos        (.position src)
        src-rem        (.remaining src)
        block-size     (long (or block-size
                                 (if (zero? src-rem)
                                   65536
                                   (get code->block-max-size (block-max-size-code src-rem)))))
        _              (when-not (contains? #{65536 262144 1048576 4194304} block-size)
                         (throw (IllegalArgumentException.
                                 (str "Invalid LZ4 block size: " block-size))))
        block-independence? (if (nil? block-independence?) true block-independence?)
        block-checksum?     (boolean block-checksum?)
        content-size?       (boolean content-size?)
        content-checksum?   (boolean content-checksum?)
        flg            (bit-or flg-version
                               (if block-independence? flg-block-independence 0)
                               (if block-checksum? flg-block-checksum 0)
                               (if content-size? flg-content-size 0)
                               (if content-checksum? flg-content-checksum 0)
                               (if dict-id flg-dict-id 0))
        bd             (bit-shift-left (block-max-size-code block-size) 4)
        content-hash   (when content-checksum? (.hash xxhash32 src src-pos src-rem 0))]
    (try
      (.order dest ByteOrder/LITTLE_ENDIAN)
      (.putInt dest magic-number)
      ;; frame descriptor
      (let [descriptor-start (.position dest)]
        (.put dest (unchecked-byte flg))
        (.put dest (unchecked-byte bd))
        (when content-size?
          (.putLong dest src-rem))
        (when dict-id
          (.putInt dest (int dict-id)))
        (.put dest (unchecked-byte (header-checksum dest descriptor-start (.position dest)))))
      ;; data blocks
      (loop [remaining src-rem]
        (if (zero? remaining)
          (do
            (.putInt dest 0) ;; EndMark
            (when content-checksum?
              (.putInt dest content-hash)))
          (let [n               (int (min remaining block-size))
                src-block-start (.position src)
                block-start     (.position dest)]
            (.putInt dest 0) ;; block size placeholder
            (let [data-start     (.position dest)
                  compressed-len (.compress compressor src src-block-start n dest data-start (.remaining dest))]
              (if (< compressed-len n)
                (do
                  (.position dest (+ data-start compressed-len))
                  (.putInt dest block-start compressed-len))
                ;; Incompressible: store the block uncompressed.
                (let [slice (.slice src)]
                  (.limit slice n)
                  (.position dest data-start)
                  (.put dest slice)
                  (.putInt dest block-start (unchecked-int (bit-or uncompressed-block-flag n)))))
              (when block-checksum?
                (.putInt dest (.hash xxhash32 dest data-start (- (.position dest) data-start) 0))))
            (.position src (+ src-block-start n))
            (recur (- remaining n)))))
      dest
      (finally
        (.order dest original-order)))))

(defn- read-frame-descriptor!
  "Reads and validates an LZ4 frame descriptor from `src` (positioned just after
   the magic number). Returns a map describing the frame."
  [^ByteBuffer src]
  (let [descriptor-start (.position src)
        flg              (bit-and (.get src) 0xFF)
        bd               (bit-and (.get src) 0xFF)
        version          (bit-and (unsigned-bit-shift-right flg 6) 0x3)]
    (when-not (= version 1)
      (throw (IllegalArgumentException.
              (str "Unsupported LZ4 frame version: " version))))
    (when-not (zero? (bit-and flg 0x02))
      (throw (IllegalArgumentException. "Reserved LZ4 FLG bit is set")))
    (when-not (zero? (bit-and bd 0x8F))
      (throw (IllegalArgumentException. "Reserved LZ4 BD bits are set")))
    (let [block-independence? (pos? (bit-and flg flg-block-independence))
          block-checksum?     (pos? (bit-and flg flg-block-checksum))
          content-size?       (pos? (bit-and flg flg-content-size))
          content-checksum?   (pos? (bit-and flg flg-content-checksum))
          dict-id?            (pos? (bit-and flg flg-dict-id))
          block-max-size      (get code->block-max-size
                                   (bit-and (unsigned-bit-shift-right bd 4) 0x7))
          _                   (when-not block-max-size
                                (throw (IllegalArgumentException.
                                        (str "Unsupported LZ4 block maximum size code: "
                                             (bit-and (unsigned-bit-shift-right bd 4) 0x7)))))
          content-size        (when content-size? (.getLong src))
          dict-id             (when dict-id? (.getInt src))
          expected-hc         (bit-and (.get src) 0xFF)
          actual-hc           (header-checksum src descriptor-start (dec (.position src)))]
      (when-not (= expected-hc actual-hc)
        (throw (IllegalArgumentException.
                (str "LZ4 frame descriptor checksum mismatch: expected "
                     expected-hc " but computed " actual-hc))))
      {:block-independence? block-independence?
       :block-checksum?     block-checksum?
       :content-checksum?   content-checksum?
       :dict-id             dict-id
       :block-max-size      block-max-size
       :content-size        content-size})))

(defn read-frame!
  "Decompresses a single LZ4 frame from the `src` ByteBuffer into the `dest`
   ByteBuffer. Returns `dest`.

   Supports the full LZ4 frame format: multiple blocks, uncompressed blocks,
   block/content checksums, content size and dictionary ids. Skippable frames
   preceding the LZ4 frame are skipped. Linked (dependent) blocks are not
   supported and throw.

   Assumes both buffers use standard endianness (LITTLE_ENDIAN is applied internally)."
  [^ByteBuffer src ^ByteBuffer dest]
  (let [original-order (.order src)
        src-pos        (.position src)]
    (try
      (.order src ByteOrder/LITTLE_ENDIAN)
      ;; skip any skippable frames
      (loop []
        (let [magic (.getInt src)]
          (cond
            (= magic magic-number) nil
            (= (bit-and magic 0xFFFFFFF0) skippable-magic-base)
            (let [skip-size (bit-and (.getInt src) 0xFFFFFFFF)
                  new-pos   (+ (.position src) skip-size)]
              (when (> new-pos (.limit src))
                (throw (IllegalArgumentException.
                        "Skippable LZ4 frame extends past the end of the buffer")))
              (.position src (int new-pos))
              (recur))
            :else
            (throw (IllegalArgumentException.
                    (str "Invalid LZ4 frame magic number: " (Long/toHexString magic)))))))
      (let [{:keys [block-independence? block-checksum? content-checksum?
                    block-max-size content-size]} (read-frame-descriptor! src)]
        (when-not block-independence?
          (throw (UnsupportedOperationException.
                  "Linked (dependent) LZ4 blocks are not supported")))
        (let [dest-start (.position dest)]
          (loop [total 0]
            (let [block-size-field (.getInt src)]
              (if (zero? block-size-field)
                ;; EndMark
                (do
                  (when content-checksum?
                    (let [expected (bit-and (.getInt src) 0xFFFFFFFF)
                          actual   (bit-and (.hash xxhash32 dest dest-start
                                                   (- (.position dest) dest-start) 0)
                                            0xFFFFFFFF)]
                      (when-not (= expected actual)
                        (throw (IllegalArgumentException.
                                (str "LZ4 content checksum mismatch: expected "
                                     expected " but computed " actual))))))
                  (when (and content-size (not= content-size total))
                    (throw (IllegalArgumentException.
                            (str "LZ4 content size mismatch: expected "
                                 content-size " but decoded " total)))))
                (let [uncompressed? (not (zero? (bit-and block-size-field uncompressed-block-flag)))
                      block-size    (bit-and block-size-field 0x7FFFFFFF)
                      data-start    (.position src)]
                  (when (> block-size block-max-size)
                    (throw (IllegalArgumentException.
                            (str "LZ4 block size " block-size
                                 " exceeds the frame maximum " block-max-size))))
                  (let [dest-pos (.position dest)]
                    (if uncompressed?
                      (do
                        (.put dest (-> src (.slice) (.limit (int block-size))))
                        (.position src (+ data-start block-size)))
                      (let [decompressed-len (.decompress decompressor src data-start block-size
                                                          dest dest-pos (.remaining dest))]
                        (.position dest (+ dest-pos decompressed-len))
                        (.position src (+ data-start block-size))))
                    (when block-checksum?
                      (let [expected (bit-and (.getInt src) 0xFFFFFFFF)
                            actual   (bit-and (.hash xxhash32 src data-start block-size 0)
                                              0xFFFFFFFF)]
                        (when-not (= expected actual)
                          (throw (IllegalArgumentException.
                                  (str "LZ4 block checksum mismatch: expected "
                                       expected " but computed " actual))))))
                    (recur (+ total (- (.position dest) dest-pos))))))))
          dest))
      (catch Exception e
        ;; Reset the read pointer on decoding failures to allow retry bounds checking
        (.position src src-pos)
        (throw e))
      (finally
        (.order src original-order)))))
