(ns mst-clj.mapping-test
  (:use clojure.test
        mst-clj.mapping))

(deftest feature-test
  (def-obj-and-id-mapping feature)
  (are [x y] (= x y)
       (feature-to-id "a") 0
       (feature-to-id "b") 1
       (feature-to-id "c") 2
       (feature-to-id "d") 3
       (feature-to-id "a") 0
       (get-max-feature-id) 4)

  (clear-feature-mapping!)

  (are [x y] (= x y)
       (feature-to-id "b") 0
       (feature-to-id "d") 1
       (get-max-feature-id) 2)

  (save-feature-to-id "/tmp/hoge.bin")
  (load-feature-to-id! "/tmp/hoge.bin")

  (are [x y] (= x y)
       (feature-to-id "a") 2
       (feature-to-id "b") 0
       (feature-to-id "d") 1
       (get-max-feature-id) 3

       (binding [*update-feature-id?* false]
         (feature-to-id "e"))
       nil
       (get-max-feature-id) 3))
