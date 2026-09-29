(ns clj-xet.merkle-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clj-xet.merkle :as merkle]
            [clj-xet.util :as util]))

(def references
  "from https://github.com/huggingface/xet-core/blob/main/xet_core_structures/src/merklehash/aggregated_hashes.rs#L273"
  (edn/read-string (slurp (io/resource "clj_xet/merkle_references.edn"))))

(defn- entries->hashes [entries]
  (map (fn [[h length]]
         {:hash (util/string-to-hash h) :length length})
       entries))

(deftest root-matches-reference-hashes
  (testing "merkle root matches the reference CAS hash for each vector"
    (doseq [[i [entries expected _file-hashes]] (map-indexed vector references)]
      (is (= expected
             (util/hash-to-string
              (merkle/root
               (entries->hashes entries))))
          (str "reference vector " i)))))

(deftest file-hash-with-salt-matches-reference
  (testing "salted file hashes match the reference values for each vector"
    (doseq [[i [entries _root file-hashes]] (map-indexed vector references)
            [salt expected] file-hashes]
      (is (= expected
             (util/hash-to-string
              (merkle/file-hash-with-salt
               (util/string-to-hash salt)
               (entries->hashes entries))))
          (str "reference vector " i ", salt " salt)))))