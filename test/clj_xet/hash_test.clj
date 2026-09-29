(ns clj-xet.hash-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-xet.util :as util])
  (:import (org.apache.commons.codec.digest Blake3)
           (java.nio.charset StandardCharsets)))

(defn- input
  "The BLAKE3 test-vector input: a repeating sequence of 251 bytes
   (0, 1, 2, ..., 249, 250, 0, 1, ...)."
  [n]
  (let [arr (byte-array n)]
    (doseq [i (range n)]
      (aset-byte arr i (unchecked-byte (mod i 251))))
    arr))

(deftest keyed-hash-matches-blake3-test-vector
  (testing "keyed hash of the 2049-byte test input with the reference key"
    (let [key (.getBytes "whats the Elvish word for friend" StandardCharsets/UTF_8)
          hasher (Blake3/initKeyedHash key)
          expected (str "9f29700902f7c86e514ddc4df1e3049f258b2472b6dd5267f61bf13983b78dd5f"
                        "9a88abfefdfa1e00b418971f2b39c64ca621e8eb37fceac57fd0c8fc8e117d43b"
                        "81447be22d5d8186f8f5919ba6bcc6846bd7d50726c06d245672c2ad4f61702c6"
                        "46499ee1173daa061ffe15bf45a631e2946d616a4c345822f1151284712f76b2b0e")]
      (is (= expected
             (util/encode-hex
              (-> hasher
                  (.update (input 2049))
                  (.doFinalize 131))))))))
