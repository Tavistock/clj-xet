(ns clj-xet.constants
  (:require [clojure.string]))

;; Minimum chunk size in bytes (8 KiB)
(def ^:const min-chunk-size 8192)
;; Maximum chunk size in bytes (128 KiB)
(def ^:const max-chunk-size 131072)

(defn parse-hex [x]
  (Long/parseUnsignedLong (clojure.string/lower-case x) 16))

(def mask (parse-hex "ffff000000000000"))

(def xorb-file
  "xet-spec-reference-files/eea25d6ee393ccae385820daed127b96ef0ea034dfb7cf6da3a950ce334b7632.xorb")
(def xorb-chunks-file
  "xet-spec-reference-files/eea25d6ee393ccae385820daed127b96ef0ea034dfb7cf6da3a950ce334b7632.xorb.chunks")
(def chunk-files
  ["xet-spec-reference-files/099cb228194fe640e36a6c7d274ee5ed3a714ccd557a0951d9b6b43a7292b5d1.chunk"
   "xet-spec-reference-files/26255591fa803b6baf25d88c315b8a6f5153d5bcfdf18ec5ef526264e0ccc907.chunk"
   "xet-spec-reference-files/b10aa1dc71c61661de92280c41a188aabc47981739b785724a099945d8dc5ce4.chunk"])

(def csv-file
  "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv")
(def xet-file-hash-file
  "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv.xet-file-hash")
(def xet-xorb-hash-file
  "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv.xet-xorb-hash")
(def shard-file
  "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv.shard")
(def dedupe-file
  "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv.shard.dedupe")
(def verification-file
  "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv.shard.verification")
(def verification-no-footer-file
  "xet-spec-reference-files/Electric_Vehicle_Population_Data_20250917.csv.shard.verification-no-footer")