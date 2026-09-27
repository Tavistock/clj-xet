(ns clj-xet.xorb
  (:require [clj-xet.hash :as hash]
            [clj-xet.gearhash :as gear-hash]
            [clj-xet.util :as util :refer [hash-to-string]]
            [clj-xet.merkle :as merkle]
            [clj-xet.lz4 :as lz4])
  (:import (java.nio ByteBuffer)
           (java.nio.channels WritableByteChannel
                              ReadableByteChannel
                              FileChannel
                              FileChannel$MapMode)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def max-xorb-size 67108864) ;; 64 MiB maximum raw payload size
(def max-xorb-chunks 8192)
(def ^:const max-chunk-size 131072) ;; Maximum chunk size in bytes (128 KiB)
(def xorb-version 0)

(defn chunk-entries [^FileChannel in-ch]
  (let [buffer (.map in-ch FileChannel$MapMode/READ_ONLY 0 (.size in-ch))]
    (loop [^long len (gear-hash/buffer-next buffer)
           offset (long 0)
           acc []]
      (if (zero? len)
        acc
        (let [chunk-hash (hash/buffer-data-hash buffer offset len)
              entry {:hash chunk-hash :length len}]
          (recur (gear-hash/buffer-next buffer)
                 (+ offset ^long len)
                 (conj acc entry)))))))

(defn chunk-lengths [^ByteBuffer buffer]
  (loop [out (transient [])]
    (if (.hasRemaining buffer)
      (let [start-offset (.position buffer)
            len          (gear-hash/buffer-next buffer)]
        (if (zero? len)
          (persistent! out)
          (recur (conj! out [start-offset len]))))
      (persistent! out))))

(defn chunks-info [^ReadableByteChannel in-ch ^WritableByteChannel out-ch]
  (doseq [{:keys [hash length]} (chunk-entries in-ch)]
    (let [line (str (hash-to-string hash) " " length "\n")]
      (.write out-ch (ByteBuffer/wrap (.getBytes line))))))

(defn xet-xorb-hash [in-ch]
  (merkle/root (chunk-entries in-ch)))

(defn xet-file-hash [hash]
  (hash/file-hash hash))

(defn verification-hash [chunk-entries & [start end]]
  (let [start (or start 0)
        end (or end (count chunk-entries))]
    (hash/verification-hash (map :hash (drop start (take end chunk-entries))))))

(def no-compression 0)
(def lz4-compression 1)
#_(def glz4-compression 2)

(defn get-3-big-end-bytes! [^java.nio.ByteBuffer buf]
  (let [b1 (Byte/toUnsignedInt (.get buf))
        b2 (Byte/toUnsignedInt (.get buf))
        b3 (Byte/toUnsignedInt (.get buf))]
    (bit-or b1
            (bit-shift-left b2 8)
            (bit-shift-left b3 16))))

(defn get-chunk-header [^java.nio.ByteBuffer buf]
  (let [version            (bit-and (.get buf) 0xFF)
        compressed-size    (get-3-big-end-bytes! buf)
        compression-scheme (bit-and (.get buf) 0xFF)
        uncompressed-size  (get-3-big-end-bytes! buf)]
    {:version version
     :compressed-size compressed-size
     :compression-scheme compression-scheme
     :uncompressed-size uncompressed-size}))

(defn no-uncompress [^ByteBuffer src ^ByteBuffer buffer uncompressed-size]
  (.put buffer (-> src
                   (.slice)
                   (.limit uncompressed-size)))
  (.position src (+ (.position src)
                    uncompressed-size)))

(defn decode [^ByteBuffer src ^WritableByteChannel dest]
  (let [^ByteBuffer buffer (ByteBuffer/allocate max-chunk-size)]
    (loop [i 0]
      (when (> (.remaining src) 8)
        (let [{:keys [compression-scheme
                      version
                      uncompressed-size]} (get-chunk-header src)
              ^ByteBuffer buffer (if (< (.capacity buffer) uncompressed-size)
                                   (ByteBuffer/allocate uncompressed-size)
                                   (.limit buffer ^long uncompressed-size))]
          (when (= xorb-version version)
            (cond
              (= compression-scheme lz4-compression) (lz4/read-frame! src buffer)
              (= compression-scheme no-compression)  (no-uncompress src buffer uncompressed-size))
            (.flip buffer)
            (while (.hasRemaining buffer)
              (.write dest buffer))
            (.clear buffer)
            (recur (inc i))))))))

(defn put-3-big-end-bytes! [^java.nio.ByteBuffer buf ^long n]
  (doto buf
    (.put (unchecked-byte (bit-and n 0xFF)))
    (.put (unchecked-byte (bit-and (bit-shift-right n 8) 0xFF)))
    (.put (unchecked-byte (bit-and (bit-shift-right n 16) 0xFF)))))

(defn put-header ^ByteBuffer
  [^ByteBuffer dest
   {:keys [version
           compressed-size
           compression-scheme
           uncompressed-size]}]
  (doto dest
    (.put (unchecked-byte version))
    (put-3-big-end-bytes! compressed-size)
    (.put (unchecked-byte compression-scheme))
    (put-3-big-end-bytes! uncompressed-size)))

(defn lz4-compress [^ByteBuffer chunk-bytes ^ByteBuffer buffer]
  (let [start-pos (.position buffer)]
    (lz4/write-frame! chunk-bytes buffer)
    (.flip buffer)
    {:compressed-length (- (.limit buffer) start-pos)
     :compressed buffer}))

(defn pick-compression-scheme-and-compress [chunk-bytes buffer]
  (-> (lz4-compress chunk-bytes buffer)
      (assoc :compression-scheme lz4-compression)))

(defn encode-buffer [^ByteBuffer chunk-bytes ^ByteBuffer dest]
  (.position dest 8)
  (let [{:keys [compression-scheme
                compressed-length]} (pick-compression-scheme-and-compress
                                     chunk-bytes
                                     dest)]
    (.position dest 0)
    (put-header dest {:version xorb-version
                      :compressed-size compressed-length
                      :compression-scheme compression-scheme
                      :uncompressed-size (.limit chunk-bytes)})
    (.position dest 0)
    dest))

(defn encode [^ByteBuffer src ^WritableByteChannel dest]
  (let [buffer (ByteBuffer/allocate (+ 8 max-chunk-size))]
    (loop [[[^int offset ^int length] & offsets] (chunk-lengths src)]
      (if-not length
        dest
        (let [chunk-bytes (.slice src offset length)]
          (encode-buffer chunk-bytes buffer)
          (while (.hasRemaining ^ByteBuffer buffer)
            (.write dest buffer))
          (.clear buffer)
          (recur offsets))))))



(comment
  (require '[clj-async-profiler.core :as prof]
           '[user :refer [time+]]
           '[clj-xet.constants :refer [csv-file]])
  (prof/profile
   {:event :alloc}
   (dotimes [_ 10]
     (with-open [^FileChannel in (util/file-read-channel csv-file)
                 ^FileChannel out (util/file-write-channel "example")
                 #_#_xin (util/file-read-channel xorb)
                 ^FileChannel in2 (util/file-read-channel "example")
                 ^FileChannel out2 (util/file-write-channel "example2")]
       (prn :start)
       (time+ (encode (util/mapped-read-buffer in)
                      out))
       #_(time+ (encode (util/mapped-read-buffer in)))
       (time+ (decode (util/mapped-read-buffer in2)
                      out2)))))

  (prof/serve-ui 8181)

  (with-open [in (util/file-read-channel csv-file)]
    (time (chunk-lengths in))
    #_(time (chunk-entries in))
    nil)

  :end)
