(ns mst-clj.word)

(defrecord Word [^String surface ^String pos-tag ^long idx ^long head])
