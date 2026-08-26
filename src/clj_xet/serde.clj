(ns clj-xet.serde
  (:import (java.nio.charset StandardCharsets)
           (java.nio ByteBuffer ByteOrder)))

(defn bytes-to-little-endian-int [^bytes bytes]
  (-> (ByteBuffer/wrap bytes)
      (.order ByteOrder/LITTLE_ENDIAN)
      (.getInt)))

(defn int-to-little-endian-bytes [^long n ^long value]
  (-> (ByteBuffer/allocate n)
      (.order ByteOrder/LITTLE_ENDIAN)
      (.putInt value)
      (.array)))

(defn le-num-bytes [value n]
  (->> (take n (iterate #(quot % 256) value))
       (map #(unchecked-byte (long (mod % 256))))
       (byte-array)))

(defn bytes-le-num [bytes]
  (reduce (fn [n byte] (+ (* n 256) (bit-and byte 0xff)))
          0
          (reverse bytes)))

(defn bytes-to-ascii [^bytes bytes]
  (String. bytes StandardCharsets/US_ASCII))

(defprotocol Serde
  (serialize [this ^java.io.OutputStream out value])
  (deserialize [this ^java.io.InputStream in]))

(defn as-ascii [^long n]
  (reify Serde
    (serialize [_ out value]
      (.write ^java.io.OutputStream out (.getBytes ^String value StandardCharsets/US_ASCII)))
    (deserialize [_ in]
      (bytes-to-ascii (.readNBytes ^java.io.InputStream in n)))))

(defn as-le-int [^long n]
  (reify Serde
    (serialize [_ out value]
      (.write ^java.io.OutputStream out (le-num-bytes value n)))
    (deserialize [_ in]
      (bytes-le-num (.readNBytes ^java.io.InputStream in n)))))

(defn as-bytes [^long n]
  (reify Serde
    (serialize [_ out value] (.write ^java.io.OutputStream out value))
    (deserialize [_ in] (.readNBytes ^java.io.InputStream in n))))

(defn as-skip [^long n]
  (reify Serde
    (serialize [_ out _value] (.write ^java.io.OutputStream out (byte-array n)))
    (deserialize [_ in] (do (.skipNBytes ^java.io.InputStream in n) nil))))

(defn deserialize-flags [^bytes bs mappings]
  (let [flag-bytes (into [] bs)]
    (reduce
     (fn [acc [^long idx k]]
       (assoc acc k (bit-test (get flag-bytes (quot idx 8))
                              (mod idx 8))))
     {}
     mappings)))

(defn serialize-flags [^long n mappings flags]
  (loop [arr (into [] (repeat n 0x00))
         [m & ms] mappings]
    (if-not m
      (byte-array arr)
      (let [[^long idx k] m]
        (recur (cond-> arr
                 (get flags k) (update (quot idx 8) #(bit-set % (mod idx 8))))
               ms)))))

(defn as-flags [n mappings]
  (reify Serde
    (serialize [_ out flags]
      (.write ^java.io.OutputStream out (serialize-flags n mappings flags)))
    (deserialize [_ in]
      (deserialize-flags (.readNBytes ^java.io.InputStream in n) mappings))))

#_(extend-type clojure.lang.PersistentVector
    Serde
    (serialize [_ _ _])
    (deserialize [_ _]))

(defn as-composite [serdes]
  (reify Serde
    (serialize [_ out value]
      (doseq [[k serde] (partition 2 serdes)]
        (serialize serde out (get value k))))

    (deserialize [_ in]
      (reduce (fn [m [k serde]] (assoc m k (deserialize serde in)))
              {}
              (partition 2 serdes)))))

(comment
  (deserialize-flags (byte-array [0xff 0x00])
                     {0 :a 8 :i})

  (serialize-flags  2
                    {0 :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j}
                    {:a true, :b true :c true :d true :e true :f true :g true :h true :i true :j true})
  :end)