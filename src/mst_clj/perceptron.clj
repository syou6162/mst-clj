(ns mst-clj.perceptron
  (:use [mst-clj.common])
  (:use [mst-clj.feature]))

(defn score-fn [weight sentence]
  (fn ^long [^long i ^long j]
    (let [^ints fv (get-in (.edge-fvs sentence) [i j])]
      (areduce fv idx result 0
               (+ result (get weight (aget ^ints fv idx) 0))))))

(defn update-weight [weight gold prediction]
  (let [nil-inc (fn ^long [x] (if (nil? x) 1 (inc x)))
        nil-dec (fn ^long [x] (if (nil? x) -1 (dec x)))
        dissoc-if-zero (fn [m k] (if (zero? (get m k)) (dissoc m k) m))]
    (if (= (map :head gold) (map :head prediction))
      weight
      (let [w (reduce (fn [w {head :head, modifier :idx}]
                        ;; the head index of root is -1 => fv is nil
                        (if-let [fv (get-in (.edge-fvs gold) [head modifier])]
                          (areduce fv idx result w
                                   (let [feature-idx (aget ^ints fv idx)]
                                     (-> (update-in result [feature-idx] nil-inc)
                                         (dissoc-if-zero feature-idx))))
                          w))
                      weight gold)]
        (reduce (fn [w {head :head, modifier :idx}]
                  ;; the head index of root is -1 => fv is nil
                  (if-let [fv (get-in (.edge-fvs prediction) [head modifier])]
                    (areduce fv idx result w
                             (let [feature-idx (aget ^ints fv idx)]
                               (-> (update-in result [feature-idx] nil-dec)
                                   (dissoc-if-zero feature-idx))))
                    w))
                w prediction)))))
