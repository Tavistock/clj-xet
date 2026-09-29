(ns clj-xet.merkle-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clj-xet.merkle :as merkle]
            [clj-xet.util :as util]))

(def references
  (edn/read-string (slurp (io/resource "clj_xet/merkle_references.edn"))))

(defn- root-hash [entries]
  (util/hash-to-string
   (merkle/root (map (fn [[h length]]
                       {:hash (util/string-to-hash h) :length length})
                     entries))))

(deftest root-matches-reference-hashes
  (testing "merkle root matches the reference CAS hash for each vector"
    (doseq [[i [entries expected _file-hashes]] (map-indexed vector references)]
      (is (= expected (root-hash entries))
          (str "reference vector " i)))))