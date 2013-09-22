(ns mst-clj.eisner
  (:import [mst_clj.sentence Sentence])
  (:use [mst-clj.common :only (deep-aget deep-aset)]))

(defn argmax [coll]
  "Return max val and its index"
  (assert (not (empty? coll)))
  (let [init-idx -1
        init-val Double/NEGATIVE_INFINITY]
    (reduce (fn [[max-idx max-val] [idx val]]
              (if (> val max-val)
                [idx val]
                [max-idx max-val]))
            [init-idx init-val]
            coll)))

(defn init-dp-table [n]
  (make-array Double/TYPE n n 2 2))

(defn eisner' [score-fn ^Sentence sentence ^doubles weight]
  (let [n (count (:words sentence))
        dp-table (init-dp-table n)
        backtrack-pointer (atom {})
        S (score-fn weight sentence)]
    (doseq [m (range 1 n), s (range n), :let [t (+ s m)], :while (< t n)]
      ;; incomplete-span
      (let [[max-idx max-val] (argmax (map (fn [q] [q (+ (deep-aget doubles dp-table s q 1 0)
                                                         (deep-aget doubles dp-table (inc q) t 0 0))])
                                           (range s t)))
            pointer (concat (get-in @backtrack-pointer [s max-idx 1 0] [])
                            (get-in @backtrack-pointer [(inc max-idx) t 0 0] []))]
        (deep-aset doubles dp-table s t 0 1 (+ (S t s) max-val))
        (swap! backtrack-pointer assoc-in [s t 0 1] (conj pointer [t s])))
      
      (let [[max-idx max-val] (argmax (map (fn [q] [q (+ (deep-aget doubles dp-table s q 1 0)
                                                         (deep-aget doubles dp-table (inc q) t 0 0))])
                                           (range s t)))
            pointer (concat (get-in @backtrack-pointer [s max-idx 1 0] [])
                            (get-in @backtrack-pointer [(inc max-idx) t 0 0] []))]
        (deep-aset doubles dp-table s t 1 1 (+ (S s t) max-val))
        (swap! backtrack-pointer assoc-in [s t 1 1] (conj pointer [s t])))
      ;; complete-span
      (let [[max-idx max-val] (argmax (map (fn [q] [q (+ (deep-aget doubles dp-table s q 0 0)
                                                         (deep-aget doubles dp-table q t 0 1))])
                                           (range s t)))
            pointer (concat (get-in @backtrack-pointer [s max-idx 0 0] [])
                            (get-in @backtrack-pointer [max-idx t 0 1] []))]
        (deep-aset doubles dp-table s t 0 0 max-val)
        (swap! backtrack-pointer assoc-in [s t 0 0] pointer))
      
      (let [[max-idx max-val] (argmax (map (fn [q] [q (+ (deep-aget doubles dp-table s q 1 1)
                                                         (deep-aget doubles dp-table q t 1 0))])
                                           (range (inc s) (inc t))))
            pointer (concat (get-in @backtrack-pointer [s max-idx 1 1] [])
                            (get-in @backtrack-pointer [max-idx t 1 0] []))]
        (deep-aset doubles dp-table s t 1 0 max-val)
        (swap! backtrack-pointer assoc-in [s t 1 0] pointer)))
    (reduce (fn [sent [head modifier]]
              (assoc-in sent [:words modifier :head] head))
            sentence
            (get-in @backtrack-pointer [0 (dec n) 1 0]))))

(defn score-fn' [^doubles weight ^Sentence sentence]
  (fn [i j]
    (let [fv (get-in sentence [:edge-fvs i j])]
      (areduce ^ints fv idx result (double 0.0)
               (+ result (aget weight
                               (aget ^ints fv idx)))))))

(def score-fn score-fn')

(defn training-score-fn
  "jのheadがiでない時にスコアを1かさ増しし、負例の重みがなるべく更新されるように
  調整する関数"
  [^doubles weight ^Sentence sentence]
  (fn [i j]
    (let [score ((score-fn' weight sentence) i j)]
      (if (= i (:head (nth (:words sentence) j)))
        score
        (inc score)))))

(def eisner-for-training (partial eisner' training-score-fn))
(def eisner (partial eisner' score-fn))
