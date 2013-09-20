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

(defn fv-diff
  "goldとpredictの素性ベクトルの差を計算する。結果はhash-mapで返す"
  [gold prediction]
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

(defn step-size-fn [^doubles weight ^Sentence gold ^Sentence prediction]
  (let [num-errors (error-count gold prediction)
        fv-diff (fv-diff gold prediction)
        in-prod (innter-product weight fv-diff)
        n (norm fv-diff)
        step-size (if (zero? n)
                    ;; goldなものでなくてもfvの次元ではexactに一致し
                    ;; normが0になってしまう場合が存在する。
                    ;; その場合0割を防ぐ必要がある
                    0.0
                    (->> (/ (- num-errors in-prod) n)
                         (max 0.0)))]
    (fn [x] (* x step-size))))

(defn update-weight
  "w = w + (phi(x_i, y_i) - phi(x_i, hat{y_i}))"
  [^doubles weight ^Sentence gold ^Sentence prediction]
  (if (= (map #(:head ^Word %) (:words gold))
         (map #(:head ^Word %) (:words prediction)))
    weight
    (let [step-size-fn (step-size-fn weight gold prediction)]
      (->> (fv-diff gold prediction)
           (map (fn [[k v]] [k (step-size-fn v)]))
           (reduce (fn [^doubles result [fv-idx v]]
                     (aset ^doubles result
                           fv-idx
                           (double (+ v (aget ^doubles result fv-idx))))
                     result)
                   weight)))))

(defn averaged-weight
  ([^doubles weight cum-count]
     (if (zero? cum-count)
       weight
       (amap weight i ret
             (/ (aget weight i) cum-count))))
  ([^doubles weight iter n]
     (averaged-weight weight (* iter n))))

(defn add-weight [^doubles w1 ^doubles w2]
  (assert (= (count w1) (count w2)))
  (amap w1 idx ret
        (+ (aget w1 idx) (aget w2 idx))))
