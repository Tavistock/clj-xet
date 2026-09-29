(ns clj-xet.shard-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clj-xet.shard :as shard]
            [clj-xet.constants])
  (:import (org.apache.commons.codec.digest DigestUtils)
           (java.io ByteArrayOutputStream ByteArrayInputStream)))

(def files [clj-xet.constants/shard-file
            clj-xet.constants/dedupe-file
            clj-xet.constants/verification-file
            clj-xet.constants/verification-no-footer-file])

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