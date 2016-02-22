(ns guten-tag.core-test
  (:refer-clojure :exclude [val])
  (:require [clojure.test :refer :all]
            [guten-tag.core :refer :all]))

(deftag a-tag [foo])

(deftest a-test
  (is (a-tag? (->a-tag 3))))
