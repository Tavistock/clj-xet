(ns clj-xet.xorb-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clj-xet.xorb :as xorb]
            [clj-xet.util :as util]
            [clj-xet.constants])
  (:import (org.apache.commons.codec.digest DigestUtils)
           (java.io ByteArrayOutputStream)
           (java.nio ByteBuffer)
           (java.nio.channels Channels)))

(def sha DigestUtils/sha256Hex)

(defn file-sha [file-name]
  (with-open [in (io/input-stream file-name)]
    (sha in)))

(defn channel-writes->bytes
  "Runs `f` with a WritableByteChannel and returns the bytes written to it."
  [f]
  (let [out (ByteArrayOutputStream.)]
    (with-open [ch (Channels/newChannel out)]
      (f ch))
    (.toByteArray out)))

(defn sha-of-channel-writes
  "Runs `f` with a WritableByteChannel and returns the SHA-256 hex of everything
   written to it."
  [f]
  (sha (channel-writes->bytes f)))

(deftest chunks-info-test
  (with-open [in (util/file-read-channel clj-xet.constants/csv-file)]
    (is (= (file-sha clj-xet.constants/xorb-chunks-file)
           (sha-of-channel-writes #(xorb/chunks-info in %))))))

(deftest hash-test
  (with-open [in (util/file-read-channel clj-xet.constants/csv-file)]
    (let [hash (xorb/xet-xorb-hash in)
          file-hash (xorb/xet-file-hash hash)]
      (is (= (slurp clj-xet.constants/xet-xorb-hash-file) (util/hash-to-string hash)))
      (is (= (slurp clj-xet.constants/xet-file-hash-file) (util/hash-to-string file-hash))))))

(deftest xorb-decode-test
  (with-open [in (util/file-read-channel clj-xet.constants/xorb-file)]
    (is (= (file-sha clj-xet.constants/csv-file)
           (sha-of-channel-writes #(xorb/decode (util/mapped-read-buffer in) %))))))

(deftest xorb-encode-decode-test
  (with-open [in (util/file-read-channel clj-xet.constants/csv-file)]
    (let [encode (ByteBuffer/wrap
                  (channel-writes->bytes
                   #(xorb/encode (util/mapped-read-buffer in) %)))]
      (is (= (file-sha clj-xet.constants/csv-file)
             (sha-of-channel-writes #(xorb/decode encode %)))))))
