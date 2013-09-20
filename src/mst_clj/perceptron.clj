(ns mst-clj.perceptron
  (:use [mst-clj.eisner :only (eisner-for-training)])
  (:import [mst_clj.word Word])
  (:import [mst_clj.sentence Sentence]))

(defn innter-product [^doubles weight fv]
  (reduce
   (fn [result [fv-idx v]]
     (+ result (* (aget weight fv-idx) v)))
   0.0
   fv))

(defn norm [fv]
  (reduce
   (fn [result [k v]] (+ result (* v v)))
   0.0
   fv))

(defn error-count [^Sentence gold ^Sentence prediction]
  (->> (map vector (rest (:words gold)) (rest (:words prediction)))
       (filter (fn [[x y]]
                 (not= (:head x) (:head y))))
       (count)))

(defn completely-correct? [^Sentence gold ^Sentence prediction]
  (zero? (error-count gold prediction)))

(defn get-fv-diff
  "goldとpredictの素性ベクトルの差を計算する。結果はhash-mapで返す"
  [^Sentence gold ^Sentence prediction]
  (let [init-m (transient {})
        next-m (reduce
                (fn [result ^Word w]
                  (let [modifier (:idx w)
                        head (:head w)
                        fv-array (get-in (:edge-fvs gold) [head modifier])]
                    (areduce ^ints fv-array idx ret result
                             (let [fv-idx (aget ^ints fv-array idx)
                                   v (get ret fv-idx 0)]
                               (assoc! ret fv-idx (inc v))))))
                init-m
                (rest (:words gold)))]
    (->> (reduce
          (fn [result ^Word w]
            (let [modifier (:idx w)
                  head (:head w)
                  fv-array (get-in (:edge-fvs prediction) [head modifier])]
              (areduce ^ints fv-array idx ret result
                       (let [fv-idx (aget ^ints fv-array idx)
                             v (get ret fv-idx 0)]
                         (assoc! ret fv-idx (dec v))))))
          next-m
          (rest (:words prediction)))
         (persistent!))))

(defn get-step-size
  [^doubles weight ^Sentence gold ^Sentence prediction fv-diff]
  (let [num-errors (error-count gold prediction)
        in-prod (innter-product weight fv-diff)
        n (norm fv-diff)
        step-size (if (zero? n)
                    ;; goldなものでなくてもfvの次元ではexactに一致し
                    ;; normが0になってしまう場合が存在する。
                    ;; その場合0割を防ぐ必要がある
                    0.0
                    (->> (/ (- num-errors in-prod) n)
                         (max 0.0)))]
    step-size))

(defn update-weight! [^doubles weight diff scale]
  (doseq [[fv-idx v] diff]
    (aset weight fv-idx (double (+ (* v scale) (aget weight fv-idx))))))

;; Ref: http://www.ss.cs.tut.ac.jp/nlp2011/nlp2010_tutorial_okanohara.pdf
(defn update-weight
  [iter ^doubles weight ^doubles cum-weight gold-sentences]
  (let [n (count gold-sentences)]
    (->> gold-sentences
         (map-indexed #(vector (inc %1) %2))
         (reduce
          (fn [[weight cum-weight] [idx gold]]
            (let [prediction (eisner-for-training gold weight)
                  step-size (get-step-size weight gold prediction)
                  diff (->> (fv-diff gold prediction)
                            (map (fn [[k v]] [k (* step-size v)])))
                  number-of-cum-examples (+ (* iter n) idx)]
              (update-weight! weight diff 1.0)
              (update-weight! cum-weight diff number-of-cum-examples)
              [weight cum-weight]))
          [weight cum-weight]))))

(defn get-averaged-weight
  "w = w_final - w_a / t"
  [cum-count ^doubles weight ^doubles cum-weight]
  (if (zero? cum-count)
    weight
    (amap weight idx ret
          (+ (aget weight idx) (/ (aget cum-weight idx) cum-count)))))
