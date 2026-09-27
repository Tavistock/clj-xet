(ns clj-xet.util
  (:require [clojure.string])
  (:import (java.nio.channels FileChannel
                              FileChannel$MapMode)
           (java.nio.file Paths StandardOpenOption)))

(defn spy [x] (prn x) x)

(defn encode-hex [^bytes bytes]
  (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn reverse-byte-order [^bytes hash]
  (byte-array
   (mapcat reverse (partition 8 hash))))

(defn hash-to-string [hash]
  (encode-hex (reverse-byte-order hash)))

(defn hex-string [s]
  (java.math.BigInteger. s 16))

(defn u64-le-bytes [value]
  (byte-array
   (map #(unchecked-byte (long (mod % 256)))
        (take 8 (iterate #(quot % 256) value)))))

(defn le [bytes]
  (reduce (fn [n byte]
            (+ (* n 256N)
               (bit-and byte 0xff)))
          0N
          (reverse bytes)))

(defn string-to-hash [s]
  (->> (partition 16 s)
       (map #(hex-string (apply str %)))
       (mapcat u64-le-bytes)
       (into-array Byte/TYPE)))

(defn u64-le [bytes]
  (when-not (= 8 (count bytes))
    (throw (ex-info "u64-le requires exactly 8 bytes"
                    {:length (count bytes)
                     :bytes bytes})))
  (reduce (fn [n byte]
            (+ (* n 256N)
               (bit-and byte 0xff)))
          0N
          (reverse bytes)))

(def hash-0
  (string-to-hash "0000000000000000000000000000000000000000000000000000000000000000"))

(def hash-1
  (string-to-hash "0000000000000000000000000000000000000000000000000000000000000001"))

(defn file-read-channel [file-name]
  (let [path (Paths/get file-name (into-array String []))
        options (into-array [StandardOpenOption/READ])]
    (FileChannel/open path options)))

(defn file-write-channel [file-name]
  (let [path (Paths/get file-name (into-array String []))
        options (into-array [StandardOpenOption/READ
                             StandardOpenOption/CREATE
                             StandardOpenOption/WRITE
                             StandardOpenOption/TRUNCATE_EXISTING])]
    (FileChannel/open path options)))

(defn mapped-read-buffer [^FileChannel in-ch]
  (.map in-ch FileChannel$MapMode/READ_ONLY 0 (.size in-ch)))

(comment
  (u64-le (byte-array [0x01 0x02 0x03 0x04
                       0x05 0x06 0x07 0x08]))

  (u64-le-bytes 2)

  (def original-hash
    (byte-array [0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]))

  (def reorder-hash
    (byte-array [7,  6,  5,  4,  3,  2,  1,  0,  15, 14, 13, 12, 11, 10,  9,  8,
                 23, 22, 21, 20, 19, 18, 17, 16, 31, 30, 29, 28, 27, 26, 25, 24]))

  (def string-repr "07060504030201000f0e0d0c0b0a090817161514131211101f1e1d1c1b1a1918")

  (= (into [] original-hash) (into [] (string-to-hash string-repr)))
  (= string-repr (hash-to-string original-hash)))
;; => 578437695752307201N
;; hexadecimal: 0x0807060504030201


;;
;; Hexdumps
;;

(def j #(clojure.string/join "" %))
(def jnl #(clojure.string/join \newline %))

(defn- hexdump-bytes
  [bytes]
  (j
   (for [b bytes]
     ;; only take lower bytes
     (let [b (mod b 256)]
       (cond
         (<= 0 b 16) (format "%02x" b)
         (< 0 b 256) (Integer/toHexString b))))))


(defn- to-ascii
  "Converts character to it's ascii representation"
  [c]
  (let [c (mod c 256)]
    (if (<= 0x1f c 0x7f)
      (char c)
      \.)))

(defn- chardump-bytes
  "Returns chardump of bytes"
  [bytes]
  (->> bytes
       (map to-ascii)
       j
       (partition-all 16)
       (map #(format "%-16s" (j %)))))

(defn- format-hexdump
  "Formats hexsump"
  [dump]
  (->> dump
       (partition-all 32)
       (map (fn [line]
              (->> line
                   (interleave (take (count dump)
                                     (for [i (iterate inc 0)]
                                       (cond
                                         (= 0 (mod i 32))       ""
                                         (and (= 1 (quot i 16))
                                              (= 0 (mod i 16))) "  "
                                         (= 0 (mod i 2))        " "
                                         :else                  ""))))
                   flatten
                   j
                   (format "%-48s"))))))

(def header1   "            +--------------------------------------------------+\n")
(def header2   "            | 0  1  2  3  4  5  6  7   8  9  a  b  c  d  e  f  |\n")
(def header3   " +----------+--------------------------------------------------+------------------+\n")
(def footer  "\n +----------+--------------------------------------------------+------------------+\n")

(defn hex-dump
  ;; taken from https://github.com/clojurewerkz/buffy/blob/v1.1.0/src/clojurewerkz/buffy/util.clj
  ;; which has a http://www.apache.org/licenses/LICENSE-2.0
  "Prints a hex representation of buffer"
  [bytes & {:keys [print] :or {print true}}]
  (let [        ;; total        (.capacity b)
        ;; total-padded (* 32 (inc (quot total 32)))
        ;; bytes        (byte-array total-padded)
        ;; _            (.getBytes b 0 bytes 0 total)
        formatted    (-> bytes hexdump-bytes format-hexdump)
        offsets      (map #(format "%08x" %) (map #(* 16 %) (range)))
        line         (repeat " | ")
        chardump     (chardump-bytes bytes)
        res          (j [header1 header2 header3
                         (->> (map vector line offsets line formatted line chardump line)
                              (map #(apply str %))
                              jnl)
                         footer])]
    (if print
      (println res)
      res)))