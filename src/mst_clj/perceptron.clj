(ns mst-clj.perceptron
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

(defn score-fn' [^doubles weight ^Sentence sentence]
  (fn [i j]
    (let [fv (get-in sentence [:edge-fvs i j])]
      (areduce ^ints fv idx result (double 0.0)
               (+ result (aget weight
                               (aget ^ints fv idx)))))))

(def score-fn score-fn')

(defn training-score-fn [^doubles weight ^Sentence sentence]
  (fn [i j]
    (let [score ((score-fn' weight sentence) i j)]
      (if (= i (:head (nth (:words sentence) j)))
        score
        (inc score)))))

(defn nil-safe-adder [base add] (if (nil? base) add (+ base add)))

(defn fv2hash-map
  "同一のkeyを持ちうるfvに対し、keyでmerge(足してmerge)した結果をhash-mapで返す関数"
  [fv]
  (reduce
   (fn [result [k v]]
     (update-in result [k] nil-safe-adder v))
   {} fv))

(defn fv-merged-by-idx [fv]
  (->> (fv2hash-map fv)
       (reduce
        (fn [result [k v]]
          (if (zero? v)
            result
            (conj result [k v])))
        [])))

(defn fv-diff [gold-fv prediction-fv]
  (->> prediction-fv
       (mapv (fn [[k v]] [k (- v)]))
       (concat gold-fv)
       (fv2hash-map)
       (fv-merged-by-idx)))

(defn concat-edge-fvs [^Sentence sent]
  (let [edge-fvs (:edge-fvs sent)]
    (->> (rest (:words sent))
         (mapv (fn [w] [(:idx w) (:head w)]))
         (reduce
          (fn [result [modifier head]]
            (into result (vec (get-in edge-fvs [head modifier]))))
          [])
         (mapv #(vector %1 1.0)))))

(defn step-size-fn [^doubles weight ^Sentence gold ^Sentence prediction]
  (let [num-errors (error-count gold prediction)
        fv-diff (fv-diff (concat-edge-fvs gold) (concat-edge-fvs prediction))
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
      (->> (fv-diff (concat-edge-fvs gold) (concat-edge-fvs prediction))
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
