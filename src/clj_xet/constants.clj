(ns clj-xet.constants
  (:require [clojure.string]))

;; Minimum chunk size in bytes (8 KiB)
(def ^:const min-chunk-size 8192)
;; Maximum chunk size in bytes (128 KiB)
(def ^:const max-chunk-size 131072)

(defn parse-hex [x]
  (Long/parseUnsignedLong (clojure.string/lower-case x) 16))

(def mask (parse-hex "ffff000000000000"))

(def ^:private reference "xet-spec-reference-files/")
(def ^:private xorb-prefix (str reference "eea25d6ee393ccae385820daed127b96ef0ea034dfb7cf6da3a950ce334b7632"))
(def xorb-file (str xorb-prefix ".xorb"))
(def xorb-chunks-file (str xorb-prefix ".xorb.chunks"))
(def csv-file (str reference "Electric_Vehicle_Population_Data_20250917.csv"))
(def csv-shard-file (str csv-file ".shard"))
(def chunk-files [(str reference "099cb228194fe640e36a6c7d274ee5ed3a714ccd557a0951d9b6b43a7292b5d1.chunk")
                  (str reference "26255591fa803b6baf25d88c315b8a6f5153d5bcfdf18ec5ef526264e0ccc907.chunk")
                  (str reference "b10aa1dc71c61661de92280c41a188aabc47981739b785724a099945d8dc5ce4.chunk")])