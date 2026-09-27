(ns clj-xet.core-test
  (:require [clojure.test :refer [deftest is]]
            [clj-xet.chunk :as chunk]
            [clj-xet.core])
  (:import [java.io StringReader]))

(deftest library-loads
  (is (some? (find-ns 'clj-xet.core))))

(deftest chunks-return-contiguous-stop-exclusive-ranges
  (let [input (apply str (take 140000 (cycle "content-defined-chunking-")))
        chunks (chunk/chunks (StringReader. input))]
    (is (seq chunks))
    (is (= 0 (:start (first chunks))))
    (is (= (count input) (:stop (last chunks))))
    (is (every? (fn [{:keys [start stop]}]
                  (<= chunk/min-chunk-size (- stop start) chunk/max-chunk-size))
                (butlast chunks)))
    (is (every? (fn [[left right]]
                  (= (:stop left) (:start right)))
                (partition 2 1 chunks)))))

(deftest chunks-handle-small-readers
  (is (= [{:start 0, :stop 0}] (chunk/chunks (StringReader. ""))))
  (is (= [{:start 0, :stop 1}] (chunk/chunks (StringReader. "2")))))
