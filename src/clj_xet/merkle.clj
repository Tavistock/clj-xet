(ns clj-xet.merkle
  (:require [clj-xet.util :refer [u64-le hash-0 hash-to-string]]
            [clj-xet.hash :as hash]))

(def ^:private mean-branching-factor 4)
(def ^:private min-children 2)
(def ^:private max-children (+ (* 2 mean-branching-factor) 1)) ;; 9

(defn ^:private next-merge-cut [hashes]
  (let [n (count hashes)
        end (min max-children n)]
    (if (<= n min-children)
      n
      (loop [[i & is] (range min-children end)]
        (if-not i
          end
          (let [{:keys [hash]} (nth hashes i)
                hash-value (u64-le (take 8 (drop 24 hash)))]
            (if (zero? (mod hash-value mean-branching-factor))
              (inc i)
              (recur is))))))))

(defn ^:private merged-hash-of-sequence [hash-pairs]
  (let [strings (map (fn [{:keys [hash length]}]
                       (str (hash-to-string hash) " : " length "\n"))
                     hash-pairs)
        new-hash (hash/internal-node-hash strings)
        new-length (reduce + (map :length hash-pairs))]
    {:hash new-hash :length new-length}))

(defn ^:private branch [entries]
  (loop [acc []
         remaining entries]
    (if-not (seq remaining)
      acc
      (let [cut (next-merge-cut remaining)]
        (recur (conj acc (merged-hash-of-sequence (take cut remaining)))
               (drop cut remaining))))))

(defn root [entries]
  (if (zero? (count entries))
    hash-0
    (loop [hv entries]
      (if (= (count hv) 1)
        (:hash (first hv))
        (recur (branch hv))))))

(defn file-hash-with-salt
  "Computes the file hash for `entries` (a sequence of `{:hash :length}` maps)
   using `salt` (a 32-byte key) as the blake3 key."
  [salt entries]
  (if (empty? entries)
    hash-0
    (hash/file-hash-with-salt salt (root entries))))
