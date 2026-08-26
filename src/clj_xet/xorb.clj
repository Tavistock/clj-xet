(ns clj-xet.xorb
  (:require [clj-xet.hash :as hash]
            [clj-xet.gear-hash :as gear-hash]
            [clj-xet.util :as util :refer [hash-to-string]]
            [clj-xet.merkle :as merkle]
            [clj-xet.serde :as sd]
            [clojure.java.io :as io])
  (:import
   (java.io InputStream
            OutputStream)
   (java.nio ByteBuffer)
   (java.nio.channels Channels
                      WritableByteChannel
                      ReadableByteChannel
                      FileChannel
                      FileChannel$MapMode)
   (net.jpountz.lz4 LZ4FrameInputStream
                    LZ4FrameOutputStream
                    LZ4FrameOutputStream$BLOCKSIZE
                    LZ4FrameOutputStream$FLG$Bits)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def max-xorb-size 67108864) ;; 64 MiB maximum raw payload size
(def max-xorb-chunks 8192)
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

(defn chunk-lengths [^FileChannel in-ch]
  (let [buffer (.map in-ch FileChannel$MapMode/READ_ONLY 0 (.size in-ch))]
    (into []
          (take-while
           (complement zero?)
           (repeatedly #(gear-hash/buffer-next buffer))))))

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

(def chunk-header-serde
  (sd/as-composite
   [:version            (sd/as-le-int 1)
    :compressed-size    (sd/as-le-int 3)
    :compression-scheme (sd/as-le-int 1)
    :uncompressed-size  (sd/as-le-int 3)]))

(defn lz4-uncompress [header ^InputStream in ^OutputStream out]
  (with-open [zin (-> (.readNBytes in (:compressed-size header))
                      java.io.ByteArrayInputStream.
                      LZ4FrameInputStream.)]
    (.transferTo zin out)))

(defn no-uncompress [header ^InputStream in ^OutputStream out]
  (with-open [in2 (io/input-stream (.readNBytes in (:compressed-size header)))]
    (.transferTo in2 out)))

(defn decode [in-ch out-ch]
  (with-open [out (Channels/newOutputStream ^WritableByteChannel out-ch)
              in (-> (Channels/newInputStream ^ReadableByteChannel in-ch)
                     (java.io.PushbackInputStream.))]
    (loop []
      (let [next-byte (.read in)]
        (when (not= -1 next-byte)
          (.unread in next-byte)
          (let [{:keys [compression-scheme
                        version] :as header} (sd/deserialize chunk-header-serde in)]
            (when-not (= xorb-version version) (prn header))
            (when (= xorb-version version)
              (cond
                (= compression-scheme lz4-compression) (lz4-uncompress header in out)
                (= compression-scheme no-compression)  (no-uncompress header in out))
              (recur))))))))

(defn lz4-compress [length ^bytes chunk-bytes]
  (with-open [baos (java.io.ByteArrayOutputStream.)]
    (with-open [^LZ4FrameOutputStream lzos (LZ4FrameOutputStream.
                                            baos
                                            LZ4FrameOutputStream$BLOCKSIZE/SIZE_256KB
                                            length
                                            (into-array [LZ4FrameOutputStream$FLG$Bits/BLOCK_INDEPENDENCE]))]
      (.write lzos chunk-bytes))
    {:compressed-length (.size baos)
     :compressed (.toByteArray baos)}))

(defn pick-compression-scheme-and-compress [length chunk-bytes]
  (-> (lz4-compress length chunk-bytes)
      (assoc :compression-scheme lz4-compression)))

(defn encode [in-ch out-ch]
  (with-open [^InputStream in (Channels/newInputStream ^ReadableByteChannel in-ch)
              ^OutputStream out (Channels/newOutputStream ^WritableByteChannel out-ch)]
    (doseq [length (chunk-lengths in-ch)]
      (let [chunk-bytes (.readNBytes in length)
            {:keys [compression-scheme
                    compressed-length
                    compressed]} (pick-compression-scheme-and-compress length
                                                                       chunk-bytes)
            header {:version xorb-version
                    :compressed-size compressed-length
                    :compression-scheme compression-scheme
                    :uncompressed-size length}]
        (sd/serialize chunk-header-serde  out header)
        (.write out ^bytes compressed)))))

(comment
  (with-open [in (util/file-read-channel csv)
              out (util/file-write-channel "example")
              #_#_xin (util/file-read-channel xorb)
              in2 (util/file-read-channel "example")
              out2 (util/file-write-channel "example2")]
    (time (encode in out))
    (time (decode in2 out2))
    #_(time (chunks-info in out)))

  (with-open [in (util/file-read-channel csv)]
    (time (chunk-lengths in))
    (time (chunk-entries in))
    nil)

  (do
    (def reference "xet-spec-reference-files/")
    (def xorb-prefix (str reference "eea25d6ee393ccae385820daed127b96ef0ea034dfb7cf6da3a950ce334b7632"))
    (def xorb (str xorb-prefix ".xorb"))
    (def xorb-chunks (str xorb-prefix ".xorb.chunks"))
    (def csv (str reference "Electric_Vehicle_Population_Data_20250917.csv"))
    (def shard (str csv ".shard"))
    (def chunk-files [(str reference "099cb228194fe640e36a6c7d274ee5ed3a714ccd557a0951d9b6b43a7292b5d1.chunk")
                      (str reference "26255591fa803b6baf25d88c315b8a6f5153d5bcfdf18ec5ef526264e0ccc907.chunk")
                      (str reference "b10aa1dc71c61661de92280c41a188aabc47981739b785724a099945d8dc5ce4.chunk")]))

  :end)
