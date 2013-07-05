(ns mst-clj.word-test
  (:use clojure.test
        mst-clj.word))

(deftest normalize-num-expr-test
  (are [x y] (= x y)
       (normalize-num-expr "100.000") "<num>"
       (normalize-num-expr "hoge") "hoge"))
