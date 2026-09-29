(ns clj-xet.shard
  "
   * Header            (48 bytes)
   * File Info Section (variable, ends with bookend)
   * CAS Info Section  (variable, ends with bookend) 
   * Footer            (200 bytes) (omitted for upload API)
   "
  (:require [clojure.java.io :as io]
            [clj-xet.serde :as sd]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def shard-magic-sequence
  (seq (byte-array [0x55, 0x69, 0x67, 0x45, 0x6a, 0x7b, 0x81, 0x57,
                    0x83, 0xa5, 0xbd, 0xd9, 0x5c, 0xcd, 0xd1, 0x4a, 0xa9])))


(def shard-header-version 2)
(def shard-footer-version 1)

(def shard-header-serde
  "
   * 0       14    Application Identifier (ASCII, null-padded)
   * 14      1     Null byte (0x00)
   * 15      17    Magic sequence (fixed)
   * 32      8     Version (64-bit unsigned, MUST be 2)
   * 40      8     Footer Size (64-bit unsigned, 0 if footer omitted)
   "
  (sd/as-composite
   [:app            (sd/as-ascii 14)
    :null-byte      (sd/as-bytes 1)
    :magic-sequence (sd/as-bytes 17)
    :version        (sd/as-le-int 8)
    :footer-size    (sd/as-le-int 8)]))

(def file-data-header-serde
  "
   * 0       32    File Hash
   * 32      4     File Flags (32-bit unsigned)
   * 36      4     Number of Entries (32-bit unsigned)
   * 40      8     Reserved (zeros)
   * File Flags:
   * 31    WITH_VERIFICATION    FileVerificationEntry present for each entry
   * 30    WITH_METADATA_EXT    FileMetadataExt present at end
   "
  (sd/as-composite
   [:hash        (sd/as-bytes 32)
    :flags       (sd/as-flags 4 {31 :with-verification
                                 30 :with-metadata-ext})
    :num-entries (sd/as-le-int 4)
    :_           (sd/as-skip 8)]))

(def file-data-entry-serde
  "
   * 0       32    CAS Hash (xorb hash)
   * 32      4     CAS Flags (32-bit unsigned, reserved, MUST be set to 0)
   * 36      4     Unpacked Segment Bytes (32-bit unsigned)
   * 40      4     Chunk Index Start (32-bit unsigned)
   * 44      4     Chunk Index End (32-bit unsigned, exclusive)"
  (sd/as-composite
   [:hash     (sd/as-bytes 32)
    :flags    (sd/as-le-int 4)
    :unpacked (sd/as-le-int 4)
    :start    (sd/as-le-int 4)
    :end      (sd/as-le-int 4)]))

(def file-verification-entry-serde
  "
   * Present only when WITH_VERIFICATION flag is set:
   * 0       32    Range Hash (verification hash)
   * 32      16    Reserved (zeros)"
  (sd/as-composite
   [:hash (sd/as-bytes 32)
    :_    (sd/as-skip 16)]))

(def file-metadata-ext-serde
  "
   * Present only when WITH_METADATA_EXT flag is set:
   * 0       32    SHA-256 Hash of file contents
   * 32      16    Reserved (zeros)"
  (sd/as-composite
   [:hash (sd/as-bytes 32)
    :_    (sd/as-skip 16)]))

(def bookend-serde
  "
   * Bytes 0-31: All 0xFF
   * Bytes 32-47: All 0x00"
  (sd/as-composite
   [:xffs (sd/as-bytes 32)
    :x00s (sd/as-bytes 16)]))

(def cas-chunk-header-serde
  "
   * 0       32    CAS Hash (xorb hash)
   * 32      4     CAS Flags (32-bit unsigned, reserved, MUST be set to 0)
   * 36      4     Number of Entries (32-bit unsigned)
   * 40      4     Num Bytes in CAS (32-bit unsigned, total uncompressed)
   * 44      4     Num Bytes on Disk (32-bit unsigned, serialized xorb size)"
  (sd/as-composite
   [:hash        (sd/as-bytes 32)
    :flags       (sd/as-le-int 4)
    :num-entries (sd/as-le-int 4)
    :cas-bytes   (sd/as-le-int 4)
    :disk-bytes  (sd/as-le-int 4)]))

(def cas-chunk-entry-serde
  "
   * 0       32    Chunk Hash
   * 32      4     Chunk Byte Range Start (32-bit unsigned)
   * 36      4     Unpacked Segment Bytes (32-bit unsigned)
   * 40      4     Flags (32-bit unsigned)
   * 44      4     Reserved (32-bit unsigned, zeros)
   
   Flags:
   * 31      GLOBAL_DEDUP_ELIGIBLE Chunk is eligible for global deduplication queries
   * 0-30    Reserved              MUST be zero"
  (sd/as-composite
   [:hash        (sd/as-bytes 32)
    :range-start (sd/as-le-int 4)
    :unpacked    (sd/as-le-int 4)
    :flags       (sd/as-flags 4 {31 :global-dedup-eligible})
    :_           (sd/as-skip 4)]))

(def shard-footer-serde
  "
   * 0       8     Version (64-bit unsigned, MUST be 1)
   * 8       8     File Info Offset (64-bit unsigned)
   * 16      8     CAS Info Offset (64-bit unsigned)
   * 24      8     File Lookup Offset (64-bit unsigned)
   * 32      8     File Lookup Num Entries (64-bit unsigned)
   * 40      8     CAS Lookup Offset (64-bit unsigned)
   * 48      8     CAS Lookup Num Entries (64-bit unsigned)
   * 56      8     Chunk Lookup Offset (64-bit unsigned)
   * 64      8     Chunk Lookup Num Entries (64-bit unsigned)
   * 72      32    Chunk Hash Key
   * 104     8     Shard Creation Timestamp (64-bit unsigned, Unix epoch seconds)
   * 112     8     Shard Key Expiry (64-bit unsigned, Unix epoch seconds)
   * 120     48    Reserved (zeros)
   * 168     8     Stored Bytes on Disk (64-bit unsigned)
   * 176     8     Materialized Bytes (64-bit unsigned)
   * 184     8     Stored Bytes (64-bit unsigned)
   * 192     8     Footer Offset (64-bit unsigned)"
  (sd/as-composite
   [:version                  (sd/as-le-int 8) ;; must be 1
    :file-info-offset         (sd/as-le-int 8)
    :cas-info-offset          (sd/as-le-int 8)
    :file-lookup-offset       (sd/as-le-int 8)
    :file-lookup-num-entries  (sd/as-le-int 8)
    :cas-lookup-offset        (sd/as-le-int 8)
    :cas-lookup-num-entries   (sd/as-le-int 8)
    :chunk-lookup-offset      (sd/as-le-int 8)
    :chunk-lookup-num-entries (sd/as-le-int 8)
    :chunk-hash-hmac-key      (sd/as-bytes 32)
    :shard-creation-timestamp (sd/as-le-int 8) ;; unix epoch seconds
    :shard-key-expiry         (sd/as-le-int 8) ;; unix epoch seconds
    :_reserved                (sd/as-skip 48)  ;; (zeros)
    :stored-bytes-on-disk     (sd/as-le-int 8)
    :materialized-bytes       (sd/as-le-int 8)
    :stored-bytes             (sd/as-le-int 8)
    :footer-offset            (sd/as-le-int 8)]))

(defn decode-shard-header [in]
  (let [{:keys [null-byte
                magic-sequence
                version]
         :as header} (sd/deserialize shard-header-serde in)]
    (assert (= (first (seq null-byte)) 0) "shard null-byte not set")
    (assert (= (seq magic-sequence) shard-magic-sequence) "shard magic-sequence not set")
    (assert (= version shard-header-version) "only shard version 2 supported")
    header))

(def bookend-marker
  (repeat 32 -1))

(defn decode-file-info
  "The file info section contains zero or more file blocks, each describing a file reconstruction. 
     The section ends with a bookend entry"
  [in]
  ;; FileDataSequenceHeader (48 bytes)
  ;; FileDataSequenceEntry entries (48 bytes each, count from header)
  ;; FileVerificationEntry entries (48 bytes each, if flag set)
  ;; FileMetadataExt (48 bytes, if flag set)
  (loop [{:keys [hash] :as header} (sd/deserialize file-data-header-serde in)
         acc []]
    (if (= bookend-marker (seq hash))
      acc
      (let [entries (->> (range (:num-entries header))
                         (map (fn [_] (sd/deserialize file-data-entry-serde in)))
                         doall)
            verification (when (:with-verification (:flags header))
                           (->> (range (:num-entries header))
                                (map (fn [_] (sd/deserialize file-verification-entry-serde in)))
                                doall))
            metadata-ext (when (:with-metadata-ext (:flags header))
                           (sd/deserialize file-metadata-ext-serde in))]
        (doseq [{:keys [^long flags]} entries]
          (assert (zero? flags) "CAS Flags must be zero"))
        (recur
         (sd/deserialize file-data-header-serde in)
         (conj acc
               {:data-header header
                :data-entries entries
                :metadata-ext metadata-ext
                :verification-entries verification}))))))

(defn decode-cas-info [in]
  ;; CASChunkSequenceHeader (48 bytes)
  ;; CASChunkSequenceEntry entries (48 bytes each, count from header)
  (loop [acc []]
    (let [{:keys [^long flags hash] :as header} (sd/deserialize cas-chunk-header-serde in)]
      (if (= bookend-marker (seq hash))
        acc
        (let [entries (->> (range (:num-entries header))
                           (map (fn [_] (sd/deserialize cas-chunk-entry-serde in)))
                           doall)]
          (assert (zero? flags) "CAS Flags must be zero")
          (doseq [{:keys [flags]} entries]
            (assert (every? false? (vals (dissoc flags :global-dedup-eligible)))
                    "invalid flags must be zero"))
          (recur
           (conj acc
                 {:cas-chunk-header  header
                  :cas-chunk-entries entries})))))))

(defn decode-shard-footer [^java.io.InputStream in]
  (let [{:keys [version] :as footer} (sd/deserialize shard-footer-serde in)]
    (assert (= 1 version))
    footer))

(defn decode
  "Sections:
   
   Header            (48 bytes)
   File Info Section (variable, ends with bookend)
   CAS Info Section  (variable, ends with bookend)
   Footer            (200 bytes) (omitted for upload API)
   "
  [^java.io.InputStream in]
  (let [header (decode-shard-header in)
        file-info (decode-file-info in)
        cas-info (decode-cas-info in)
        footer (when (pos? (:footer-size header))
                 (decode-shard-footer in))]
    (assert (= -1 (.read in)) "end of file")
    {:header header
     :file-info file-info
     :cas-info cas-info
     :footer footer}))

(def bookend-data
  {:xffs (byte-array (repeat 32 0xff))
   :x00s (byte-array (repeat 16 0x00))})

(defn encode
  "Sections:
   
   Header            (48 bytes)
   File Info Section (variable, ends with bookend)
   CAS Info Section  (variable, ends with bookend)
   Footer            (200 bytes) (omitted for upload API)
   "
  [^java.io.OutputStream out value]
  (let [{:keys [file-info header footer cas-info]} value]
    ;; header
    (sd/serialize shard-header-serde out header)

    ;; file-info
    (doseq [{:keys [data-header data-entries verification-entries metadata-ext]} file-info]
      (sd/serialize file-data-header-serde out data-header)
      (doseq [entry data-entries] (sd/serialize file-data-entry-serde out entry))
      (when verification-entries
        (doseq [entry verification-entries]
          (sd/serialize file-verification-entry-serde out entry)))
      (when metadata-ext (sd/serialize file-metadata-ext-serde out metadata-ext)))

    (sd/serialize bookend-serde out bookend-data)

    ;; cas-info
    (doseq [{:keys [cas-chunk-header cas-chunk-entries]} cas-info]
      (sd/serialize cas-chunk-header-serde out cas-chunk-header)
      (doseq [entry cas-chunk-entries] (sd/serialize cas-chunk-entry-serde out entry)))

    (sd/serialize bookend-serde out bookend-data)

    ;; footer
    (when footer (sd/serialize shard-footer-serde out footer))))
