(ns clj-xet.xorb-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clj-xet.xorb :as xorb]
            [clj-xet.util :as util])
  (:import (org.apache.commons.codec.digest DigestUtils)
           (org.apache.commons.codec.binary Hex)
           (java.nio ByteBuffer)
           (java.nio.channels Pipe)))

(def folder        "xet-spec-reference-files/")
(def csv-file      (str folder "Electric_Vehicle_Population_Data_20250917.csv"))
(def xorb-hash     "eea25d6ee393ccae385820daed127b96ef0ea034dfb7cf6da3a950ce334b7632")
(def xorb-file     (str folder xorb-hash ".xorb"))
(def chunks-file   (str folder xorb-hash ".xorb.chunks"))
(def xet-file-hash (str csv-file ".xet-file-hash"))
(def xet-xorb-hash (str csv-file ".xet-xorb-hash"))

(def sha DigestUtils/sha256Hex)

(defn file-sha [file-name]
  (with-open [in (io/input-stream file-name)]
    (sha in)))

(defn channel-sha
  [^java.nio.channels.ReadableByteChannel channel]
  (let [digest (DigestUtils/getSha256Digest)
        buffer (ByteBuffer/allocate 4096)]
    (while (> (.read channel buffer) 0)
      (.flip buffer)
      (.update digest buffer)
      (.clear buffer))
    (Hex/encodeHexString (.digest digest))))

(deftest chunks-info-test
  (let [pipe (Pipe/open)]
    (with-open [in (util/file-read-channel csv-file)
                source (.source pipe)]
      (let [decode-future (future (with-open [sink (.sink pipe)]
                                    (xorb/chunks-info in sink)))]
        (is (= (file-sha chunks-file) (channel-sha source)))
        @decode-future))))

(deftest hash-test
  (with-open [in (util/file-read-channel csv-file)]
    (let [hash (xorb/xet-xorb-hash in)
          file-hash (xorb/xet-file-hash hash)]
      (is (= (slurp xet-xorb-hash) (util/hash-to-string hash)))
      (is (= (slurp xet-file-hash) (util/hash-to-string file-hash))))))

(deftest xorb-decode-test
  (let [pipe (Pipe/open)]
    (with-open [in (util/file-read-channel xorb-file)
                source (.source pipe)]
      (let [decode-future (future (with-open [sink (.sink pipe)]
                                    (xorb/decode (util/mapped-read-buffer in) sink)))]
        (is (= (file-sha csv-file) (channel-sha source)))
        @decode-future))))

(deftest xorb-encode-decode-test
  (let [decode-pipe (Pipe/open)]
    (with-open [in (util/file-read-channel csv-file)
                decoded-source (.source decode-pipe)]
      (let [encode (xorb/encode (util/mapped-read-buffer in))
            decode-future (future
                            (with-open [decode-sink (.sink decode-pipe)]
                              (xorb/decode encode decode-sink)))]
        (is (= (file-sha csv-file) (channel-sha decoded-source)))
        @decode-future))))
