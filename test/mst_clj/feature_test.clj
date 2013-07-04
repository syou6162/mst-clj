(ns mst-clj.feature-test
  (:use clojure.test
        mst-clj.feature))

(deftest normalize-test
  (are [x y] (= x y)
       (normalize "100.000") "<num>"
       (normalize "hoge") "hoge"))
