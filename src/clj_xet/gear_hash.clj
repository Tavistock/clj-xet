(ns clj-xet.gear-hash
  (:require [clj-xet.gear-hash-table :refer [table]]
            [clojure.string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; Minimum chunk size in bytes (8 KiB)
(def ^:const min-chunk-size 8192)
;; Maximum chunk size in bytes (128 KiB)
(def ^:const max-chunk-size 131072)

(defn parse-hex [x]
  (Long/parseUnsignedLong (clojure.string/lower-case x) 16))

(def mask (parse-hex "ffff000000000000"))

(defn next-length
  "Consumes input-stream and returns gear hash chunk length"
  ([^java.io.InputStream in] (next-length in 0 table mask))
  ([^java.io.InputStream in ^long hash ^"[J" table ^long mask]
   (loop [h hash
          i 0]
     (let [b (.read in)]
       (if (neg? b)
         i
         (let [i (inc i)
               h (unchecked-add (bit-shift-left h 1) (aget table b))]
           (if (or (>= i max-chunk-size)
                   (and (>= i min-chunk-size)
                        (zero? (bit-and h mask))))
             i
             (recur h i))))))))

(defn buffer-next
  "Consumes buffer and returns gear hash chunk length"
  ([^java.nio.ByteBuffer buffer] (buffer-next buffer 0 table mask))
  ([^java.nio.ByteBuffer buffer ^long hash ^"[J" table ^long mask]
   (loop [h hash
          i 0]
     (if-not (.hasRemaining buffer)
       i
       (let [b (.get buffer)
             i (inc i)
             h (unchecked-add (bit-shift-left h 1) (aget table b))]
         (if (or (>= i max-chunk-size)
                 (and (>= i min-chunk-size)
                      (zero? (bit-and h mask))))
           i
           (recur h i)))))))

(comment
  *e
  (def chunk-files
    ["xet-spec-reference-files/099cb228194fe640e36a6c7d274ee5ed3a714ccd557a0951d9b6b43a7292b5d1.chunk"
     "xet-spec-reference-files/26255591fa803b6baf25d88c315b8a6f5153d5bcfdf18ec5ef526264e0ccc907.chunk"
     "xet-spec-reference-files/b10aa1dc71c61661de92280c41a188aabc47981739b785724a099945d8dc5ce4.chunk"])

  (map #(.length (clojure.java.io/as-file %)) chunk-files)

  (list 61389 106099 131072)

  (let [file "/Users/travis/code/clj-xet/xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv"]
    #_(chunks file)
    (with-open [in (java.io.BufferedInputStream. (clojure.java.io/input-stream file))]
      (prn (type in))
      (time (take 3 (into [] (take-while some? (repeatedly #(next-length in 0 table mask))))))))

  ;;"Elapsed time: 2894.671253 msecs"
  ;;(131072 106099 61389)

  (* 1.0 (/ 63527244 max-chunk-size))

  :end)