(ns clj-xet.shard-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clj-xet.shard :as shard])
  (:import (org.apache.commons.codec.digest DigestUtils)
           (java.io ByteArrayOutputStream ByteArrayInputStream)))

(def shard-file "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv.shard")
(def dedupe-file    (str shard-file ".dedupe"))
(def veri-file      (str shard-file ".verification"))
(def no-footer-file (str shard-file ".verification-no-footer"))
(def files [shard-file dedupe-file veri-file no-footer-file])

(def sha DigestUtils/sha256Hex)

(defn file-sha [file-name]
  (with-open [in (io/input-stream file-name)]
    (sha in)))

(deftest encode-decode
  (testing "round trip results in same file after decode then encode"
    (doseq [file files]
      (with-open [in (io/input-stream file)
                  out (ByteArrayOutputStream.)]
        (shard/encode out (shard/decode in))
        (with-open [bais (ByteArrayInputStream. (.toByteArray out))]
          (is (= (file-sha file) (sha bais))))))))