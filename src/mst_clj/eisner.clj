(ns mst-clj.eisner
  (:use [mst-clj.label :only (label-to-id-mapping)])
  (:use [mst-clj.common :only (deep-aget deep-aset)])
  (:import [mst_clj.sentence Sentence])
  (:import [mst_clj.word Word]))

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

(defn eisner' [argmax-score-label-fn ^doubles weight ^Sentence sentence]
  (let [n (count (:words sentence))
        dp-table (init-dp-table n)
        backtrack-pointer (atom {})
        S (argmax-score-label-fn weight sentence)]
    (doseq [m (range 1 n), s (range n), :let [t (+ s m)], :while (< t n)]
      ;; incomplete-span
      (let [[max-idx max-val] (argmax (map (fn [q] [q (+ (deep-aget doubles dp-table s q 1 0)
                                                         (deep-aget doubles dp-table (inc q) t 0 0))])
                                           (range s t)))
            pointer (concat (get-in @backtrack-pointer [s max-idx 1 0] [])
                            (get-in @backtrack-pointer [(inc max-idx) t 0 0] []))]
        (deep-aset doubles dp-table s t 0 1 (+ (second (S t s)) max-val))
        (swap! backtrack-pointer assoc-in [s t 0 1] (conj pointer [t s])))
      
      (let [[max-idx max-val] (argmax (map (fn [q] [q (+ (deep-aget doubles dp-table s q 1 0)
                                                         (deep-aget doubles dp-table (inc q) t 0 0))])
                                           (range s t)))
            pointer (concat (get-in @backtrack-pointer [s max-idx 1 0] [])
                            (get-in @backtrack-pointer [(inc max-idx) t 0 0] []))]
        (deep-aset doubles dp-table s t 1 1 (+ (second (S s t)) max-val))
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
              (let [label (first (S head modifier))]
                (-> sent
                    (assoc-in  [:words modifier :head] head)
                    (assoc-in  [:words modifier :label] label))))
            sentence
            (get-in @backtrack-pointer [0 (dec n) 1 0]))))

(defn score-label-pairs [^doubles weight ^Sentence sentence i j]
  (->> (label-to-id-mapping)
       (map (fn [[label idx]]
              (if-let [fv (-> sentence :edge-fvs (get-in [i label j]))]
                [label
                 (areduce
                  ^ints fv idx ret (double 0.0)
                  (+ ret (aget weight (aget ^ints fv idx))))])))
       (remove nil?)
       (vec)))

(defn argmax-score-label-fn [^doubles weight ^Sentence sentence]
  (fn [i j]
    (->> (score-label-pairs weight sentence i j)
         (argmax))))

(defn training-argmax-score-label-fn [^doubles weight ^Sentence sentence]
  (fn [i j]
    (->> (score-label-pairs weight sentence i j)
         (mapv (fn [[label score]]
                 (let [^Word w (nth (:words sentence) j)]
                   (if (and (= i (.head w)) (= label (.label w)))
                     [label score]
                     [label (inc score)]))))
         (argmax))))

(def eisner-for-training (partial eisner' training-argmax-score-label-fn))
(def eisner (partial eisner' argmax-score-label-fn))
