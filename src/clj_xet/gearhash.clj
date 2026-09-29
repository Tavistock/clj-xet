(ns clj-xet.gearhash
  (:require [clj-xet.gearhash-table :refer [table]]
            [clj-xet.constants :refer [mask
                                       max-chunk-size
                                       min-chunk-size]]
            [clojure.string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn next-length ^long
  ([^java.nio.ByteBuffer buffer]
   (next-length buffer 0 table mask))
  ([^java.nio.ByteBuffer buffer ^long hash]
   (next-length buffer hash table mask))
  ([^java.nio.ByteBuffer buffer ^long hash ^"[J" table ^long mask]
   (loop [h hash
          i 0]
     (if-not (.hasRemaining buffer)
       i
       (let [b (.get buffer)
             i (inc i)
             h (unchecked-add (bit-shift-left h 1) (aget table b))]
         (if (zero? (bit-and h mask))
           i
           (recur h i)))))))

(defn next-chunk-length ^long [^java.nio.ByteBuffer buffer]
  (let [slice (.slice buffer
                      (.position buffer)
                      (min (.remaining buffer) max-chunk-size))
        size (loop [size (long 0)
                    ^long length (next-length slice)]
               (let [size (+ size length)]
                 (if (>= size min-chunk-size)
                   size
                   (recur size (next-length slice)))))]
    (.position buffer (+ ^long size (.position buffer)))
    size))

(defn chunk-lengths [^java.nio.ByteBuffer buffer]
  (loop [out (transient [])]
    (if (.hasRemaining buffer)
      (let [start-offset (.position buffer)
            len          (next-chunk-length buffer)]
        (recur (conj! out [start-offset len])))
      (persistent! out))))


(comment
  (require '[clj-xet.util :as util]
           '[clj-async-profiler.core :as prof]
           '[user :refer [time+]])

  (prof/profile
   #_{:event :alloc}
   (dotimes [_ 10]
     (let [x (util/mapped-read-buffer (util/file-read-channel clj-xet.constants/csv-file))]
       (time+ (prn (count (chunk-lengths x)))))))

  :end)