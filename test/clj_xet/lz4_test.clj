(ns clj-xet.lz4-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-xet.lz4 :as lz4]
            [clj-xet.util :as util]
            [clj-xet.constants])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.nio.file Files Paths)
           (java.util Random)))

(defn- round-trip
  "Writes `data` as an LZ4 frame with `opts` and reads it back, returning the
   decoded bytes."
  [^bytes data & opts]
  (let [src  (ByteBuffer/wrap data)
        dest (ByteBuffer/allocate (+ (lz4/max-frame-size (alength data)) 256))]
    (apply lz4/write-frame! src dest opts)
    (.flip dest)
    (let [out (ByteBuffer/allocate (max 1 (alength data)))]
      (lz4/read-frame! dest out)
      (.flip out)
      (let [result (byte-array (.remaining out))]
        (.get out result)
        result))))

(defn- random-bytes [n seed]
  (let [r (Random. seed)
        b (byte-array n)]
    (.nextBytes r b)
    b))

(defn- bytes= [^bytes a ^bytes b]
  (and (= (alength a) (alength b))
       (java.util.Arrays/equals a b)))

(defn- frame-buffer
  "Writes `data` as an LZ4 frame with `opts` and returns a flipped ByteBuffer
   containing the raw frame bytes."
  ^ByteBuffer [^bytes data & opts]
  (let [dest (ByteBuffer/allocate (+ (lz4/max-frame-size (alength data)) 256))]
    (apply lz4/write-frame! (ByteBuffer/wrap data) dest opts)
    (.flip dest)
    dest))

(deftest round-trip-test
  (testing "empty input"
    (is (bytes= (byte-array 0) (round-trip (byte-array 0)))))
  (testing "small compressible input"
    (let [data (.getBytes (apply str (repeat 1000 "hello world ")))]
      (is (bytes= data (round-trip data)))))
  (testing "input larger than the 64 KiB default block size"
    (let [data (.getBytes (apply str (repeat 20000 "abcdefghij")))]
      (is (bytes= data (round-trip data)))))
  (testing "incompressible input (uncompressed block path)"
    (let [data (random-bytes 100000 42)]
      (is (bytes= data (round-trip data))))))

(deftest options-round-trip-test
  (let [data (.getBytes (apply str (repeat 5000 "the quick brown fox ")))]
    (testing "block checksum"
      (is (bytes= data (round-trip data :block-checksum? true))))
    (testing "content checksum"
      (is (bytes= data (round-trip data :content-checksum? true))))
    (testing "content size"
      (is (bytes= data (round-trip data :content-size? true))))
    (testing "dictionary id"
      (is (bytes= data (round-trip data :dict-id 0x12345678))))
    (testing "all optional fields together"
      (is (bytes= data (round-trip data
                                   :block-checksum? true
                                   :content-checksum? true
                                   :content-size? true
                                   :dict-id 7))))))

(deftest multi-block-test
  (let [data (random-bytes 200000 7)]
    (testing "explicit 64 KiB blocks"
      (is (bytes= data (round-trip data :block-size 65536))))
    (testing "explicit 64 KiB blocks with block checksums"
      (is (bytes= data (round-trip data :block-size 65536 :block-checksum? true))))
    (testing "explicit 64 KiB blocks with content checksum and size"
      (is (bytes= data (round-trip data
                                   :block-size 65536
                                   :content-checksum? true
                                   :content-size? true))))))

(deftest block-size-selection-test
  (testing "the smallest block size that fits the input is chosen"
    (doseq [[n expected-bd] [[100 0x40] [65536 0x40] [65537 0x50]
                             [262144 0x50] [262145 0x60] [1048576 0x60]
                             [1048577 0x70]]]
      (let [data (byte-array n)
            src  (ByteBuffer/wrap data)
            dest (ByteBuffer/allocate (+ (lz4/max-frame-size n) 256))]
        (lz4/write-frame! src dest)
        (.flip dest)
        (is (= expected-bd (bit-and (.get dest 5) 0xFF))
            (str "block size for " n " bytes"))))))

(deftest skippable-frame-test
  (let [data  (.getBytes "payload that should survive a skippable frame")
        frame (ByteBuffer/allocate (+ (lz4/max-frame-size (alength data)) 64))]
    ;; skippable frame: magic 0x184D2A50, size 4, 4 bytes of user data
    (.order frame ByteOrder/LITTLE_ENDIAN)
    (.putInt frame 0x184D2A50)
    (.putInt frame 4)
    (.putInt frame (unchecked-int 0xDEADBEEF))
    (lz4/write-frame! (ByteBuffer/wrap data) frame)
    (.flip frame)
    (let [out (ByteBuffer/allocate (alength data))]
      (lz4/read-frame! frame out)
      (.flip out)
      (let [result (byte-array (.remaining out))]
        (.get out result)
        (is (bytes= data result))))))

(deftest explicit-block-sizes-test
  (testing "explicitly requested block sizes round-trip"
    (let [data (random-bytes 300000 11)]
      (doseq [bs [65536 262144 1048576 4194304]]
        (is (bytes= data (round-trip data :block-size bs))
            (str "block size " bs))))))

(deftest multiple-skippable-frames-test
  (testing "several skippable frames (including alternate magics) are skipped"
    (let [data  (.getBytes "payload after two skippable frames")
          frame (ByteBuffer/allocate (+ (lz4/max-frame-size (alength data)) 64))]
      (.order frame ByteOrder/LITTLE_ENDIAN)
      ;; first skippable frame, magic 0x184D2A50
      (.putInt frame 0x184D2A50)
      (.putInt frame 2)
      (.putShort frame (unchecked-short 0x1234))
      ;; second skippable frame, alternate magic 0x184D2A5F
      (.putInt frame 0x184D2A5F)
      (.putInt frame 0)
      (lz4/write-frame! (ByteBuffer/wrap data) frame)
      (.flip frame)
      (let [out (ByteBuffer/allocate (alength data))]
        (lz4/read-frame! frame out)
        (.flip out)
        (let [result (byte-array (.remaining out))]
          (.get out result)
          (is (bytes= data result)))))))

(deftest header-checksum-validation-test
  (let [data  (.getBytes "checksum me")
        frame (ByteBuffer/allocate (+ (lz4/max-frame-size (alength data)) 64))]
    (lz4/write-frame! (ByteBuffer/wrap data) frame)
    (.flip frame)
    ;; corrupt the header checksum byte (offset 6)
    (.put frame 6 (unchecked-byte (bit-xor (.get frame 6) 0xFF)))
    (is (thrown-with-msg? IllegalArgumentException #"descriptor checksum"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest content-checksum-validation-test
  (let [data  (.getBytes "content checksum")
        frame (ByteBuffer/allocate (+ (lz4/max-frame-size (alength data)) 64))]
    (lz4/write-frame! (ByteBuffer/wrap data) frame :content-checksum? true)
    (.flip frame)
    ;; corrupt the trailing content checksum
    (let [last-idx (dec (.limit frame))]
      (.put frame last-idx (unchecked-byte (bit-xor (.get frame last-idx) 0xFF))))
    (is (thrown-with-msg? IllegalArgumentException #"content checksum"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest linked-blocks-unsupported-test
  (let [data  (.getBytes "linked blocks")
        frame (ByteBuffer/allocate (+ (lz4/max-frame-size (alength data)) 64))]
    (lz4/write-frame! (ByteBuffer/wrap data) frame :block-independence? false)
    (.flip frame)
    (is (thrown? UnsupportedOperationException
                 (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest reference-frame-test
  (testing "the first frame of the reference xorb decodes to the first chunk"
    (with-open [in (util/file-read-channel clj-xet.constants/xorb-file)]
      (let [src (util/mapped-read-buffer in)]
        (.position src 8) ;; skip the xorb chunk header
        (let [out (ByteBuffer/allocate 131072)]
          (lz4/read-frame! src out)
          (.flip out)
          (let [decoded (byte-array (.remaining out))
                _       (.get out decoded)
                ;; the first chunk in the xorb is b10aa1dc..., the last entry
                expected (Files/readAllBytes
                          (Paths/get (last clj-xet.constants/chunk-files)
                                     (into-array String [])))]
            (is (bytes= expected decoded))))))))

;;
;; Error and edge-case paths
;;

(deftest write-invalid-block-size-test
  (testing "a block size that is not a valid LZ4 block maximum is rejected"
    (is (thrown-with-msg? IllegalArgumentException #"Invalid LZ4 block size"
                          (lz4/write-frame! (ByteBuffer/wrap (byte-array 10))
                                            (ByteBuffer/allocate 128)
                                            :block-size 12345)))))

(deftest write-oversized-input-test
  (testing "input larger than the 4 MiB maximum is rejected"
    (let [data (byte-array (inc 4194304))]
      (is (thrown-with-msg? IllegalArgumentException #"exceeds the 4 MiB maximum"
                            (lz4/write-frame! (ByteBuffer/wrap data)
                                              (ByteBuffer/allocate 64)))))))

(deftest read-invalid-magic-test
  (let [frame (frame-buffer (.getBytes "hello"))]
    (.put frame 0 (unchecked-byte 0x00)) ;; corrupt the magic number
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"Invalid LZ4 frame magic"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-unsupported-version-test
  (let [frame (frame-buffer (.getBytes "hello"))]
    ;; FLG byte is at offset 4; clear the version bits (7-6)
    (.put frame 4 (unchecked-byte (bit-and (.get frame 4) 0x3F)))
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"Unsupported LZ4 frame version"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-reserved-flg-bit-test
  (let [frame (frame-buffer (.getBytes "hello"))]
    ;; set the reserved FLG bit (bit 1)
    (.put frame 4 (unchecked-byte (bit-or (.get frame 4) 0x02)))
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"Reserved LZ4 FLG bit"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-reserved-bd-bits-test
  (let [frame (frame-buffer (.getBytes "hello"))]
    ;; set a reserved BD bit (bit 7)
    (.put frame 5 (unchecked-byte (bit-or (.get frame 5) 0x80)))
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"Reserved LZ4 BD bits"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-unsupported-block-max-size-test
  (let [frame (frame-buffer (.getBytes "hello"))]
    ;; BD bits 6-4 = 0 is an unused block maximum size code
    (.put frame 5 (unchecked-byte (bit-and (.get frame 5) 0x8F)))
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"Unsupported LZ4 block maximum size"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-content-size-mismatch-test
  (let [frame (frame-buffer (.getBytes "hello world") :content-size? true)]
    ;; content size is an 8-byte LE value at offset 6; bump it by one
    (.put frame 6 (unchecked-byte (inc (.get frame 6))))
    ;; recompute the descriptor checksum (offset 14) so only the size is wrong
    (.put frame 14 (unchecked-byte (#'lz4/header-checksum frame 4 14)))
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"content size mismatch"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-block-size-exceeds-max-test
  (let [frame (frame-buffer (.getBytes "hello"))]
    ;; block size field is at offset 7; set it beyond the 64 KiB frame maximum
    (.order frame ByteOrder/LITTLE_ENDIAN)
    (.putInt frame 7 100000)
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"exceeds the frame maximum"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-block-checksum-mismatch-test
  (let [frame (frame-buffer (.getBytes "block checksum") :block-checksum? true)]
    ;; the block checksum is the 4 bytes right after the compressed block; the
    ;; frame is [7-byte header][4-byte size][block][4-byte checksum][4-byte end]
    ;; corrupt the last byte of the block checksum
    (let [checksum-last (- (.limit frame) 5)]
      (.put frame checksum-last (unchecked-byte (bit-xor (.get frame checksum-last) 0xFF))))
    (.position frame 0)
    (is (thrown-with-msg? IllegalArgumentException #"block checksum mismatch"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-skippable-frame-past-end-test
  (let [frame (ByteBuffer/allocate 16)]
    (.order frame ByteOrder/LITTLE_ENDIAN)
    (.putInt frame 0x184D2A50)
    (.putInt frame 1000) ;; claims 1000 bytes of user data that aren't there
    (.flip frame)
    (is (thrown-with-msg? IllegalArgumentException #"extends past the end"
                          (lz4/read-frame! frame (ByteBuffer/allocate 64))))))

(deftest read-resets-position-on-failure-test
  (testing "a failed read leaves the source position unchanged"
    (let [frame (frame-buffer (.getBytes "hello"))]
      (.put frame 0 (unchecked-byte 0x00)) ;; corrupt the magic
      (.position frame 3)
      (is (thrown? IllegalArgumentException
                   (lz4/read-frame! frame (ByteBuffer/allocate 64))))
      (is (= 3 (.position frame))))))
