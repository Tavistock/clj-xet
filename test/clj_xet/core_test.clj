(ns clj-xet.core-test
  (:require [clojure.test :refer [deftest is]]
            [clj-xet.core]))

(deftest library-loads
  (is (some? (find-ns 'clj-xet.core))))