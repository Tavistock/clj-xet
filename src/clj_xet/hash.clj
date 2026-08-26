(ns clj-xet.hash
  (:import (org.apache.commons.codec.digest Blake3)
           (java.nio.charset StandardCharsets)
           (java.nio ByteBuffer)))

(def ^bytes data-key
  (byte-array
   [0x66, 0x97, 0xf5, 0x77, 0x5b, 0x95, 0x50, 0xde,
    0x31, 0x35, 0xcb, 0xac, 0xa5, 0x97, 0x18, 0x1c,
    0x9d, 0xe4, 0x21, 0x10, 0x9b, 0xeb, 0x2b, 0x58,
    0xb4, 0xd0, 0xb0, 0x4b, 0x93, 0xad, 0xf2, 0x29]))

(def ^bytes internal-node-key
  (byte-array
   [0x01, 0x7e, 0xc5, 0xc7, 0xa5, 0x47, 0x29, 0x96,
    0xfd, 0x94, 0x66, 0x66, 0xb4, 0x8a, 0x02, 0xe6,
    0x5d, 0xdd, 0x53, 0x6f, 0x37, 0xc7, 0x6d, 0xd2,
    0xf8, 0x63, 0x52, 0xe6, 0x4a, 0x53, 0x71, 0x3f]))

(def ^bytes verification-key
  (byte-array
   [0x7f, 0x18, 0x57, 0xd6, 0xce, 0x56, 0xed, 0x66,
    0x12, 0x7f, 0xf9, 0x13, 0xe7, 0xa5, 0xc3, 0xf3,
    0xa4, 0xcd, 0x26, 0xd5, 0xb5, 0xdb, 0x49, 0xe6,
    0x41, 0x24, 0x98, 0x7f, 0x28, 0xfb, 0x94, 0xc3]))

(def ^bytes zero-key
  (byte-array
   [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))


(defn buffer-data-hash
  [^ByteBuffer buf ^long offset ^long length]
  (let [^Blake3 hasher (Blake3/initKeyedHash data-key)
        arr (byte-array length)
        orig-pos (.position buf)]
    (try
      (.position buf offset)
      (.get buf arr 0 length)
      (.update hasher arr)
      (finally
        (.position buf orig-pos)))
    (.doFinalize hasher 32)))

(defn internal-node-hash [xs]
  (let [^Blake3 hasher (Blake3/initKeyedHash internal-node-key)]
    (doseq [^String s xs]
      (.update hasher (.getBytes s StandardCharsets/UTF_8)))
    (.doFinalize hasher 32)))

(defn file-hash [hash]
  (let [^Blake3 hasher (Blake3/initKeyedHash zero-key)]
    (.update hasher hash)
    (.doFinalize hasher 32)))

(defn verification-hash [hashes]
  (let [^Blake3 hasher (Blake3/initKeyedHash verification-key)]
    (doseq [x hashes]
      (.update hasher x))
    (.doFinalize hasher 32)))

(comment
  (def chunk-files
    ["xet-spec-reference-files/099cb228194fe640e36a6c7d274ee5ed3a714ccd557a0951d9b6b43a7292b5d1.chunk"
     "xet-spec-reference-files/26255591fa803b6baf25d88c315b8a6f5153d5bcfdf18ec5ef526264e0ccc907.chunk"
     "xet-spec-reference-files/b10aa1dc71c61661de92280c41a188aabc47981739b785724a099945d8dc5ce4.chunk"])

  (import '[java.nio.charset StandardCharsets])

  (defn encode-hex [^bytes bytes]
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

  (defn input [n]
    (let [arr (byte-array n)]
      (doseq [i (range n)]
        (aset-byte arr i (unchecked-byte (mod i 251))))
      arr))

  (def a "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262e00f03e7b69af26b7faaf09fcd333050338ddfe085b8cc869ca98b206c08243a26f5487789e8f660afe6c99ef9e0c52b92e7393024a80459cf91f476f9ffdbda7001c22e159b402631f277ca96f2defdf1078282314e763699a31c5363165421cce14d")

  (let [KEY (.getBytes "whats the Elvish word for friend" StandardCharsets/UTF_8)
        keyedHasher (Blake3/initKeyedHash KEY)
        input-byte-array (input 2049)]
    (-> keyedHasher
        (.update input-byte-array)
        (.doFinalize 131)
        (encode-hex))))