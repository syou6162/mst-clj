(ns mst-clj.perceptron
  (:use [mst-clj.feature]))

(defn score-fn [weight sentence]
  (fn [i j]
    (let [fv (get-in (.edge-fvs sentence) [i j])]
      (reduce (fn [result feature]
                (+ result (get weight feature 0.0)))
              0.0 fv))))

(defn update-weight [weight gold prediction]
  (let [nil-inc (fn [x] (if (nil? x) 1 (inc x)))
        nil-dec (fn [x] (if (nil? x) -1 (dec x)))]
    (if (= (map :head gold) (map :head prediction))
      weight
      (let [w (reduce (fn [w {head :head, modifier :idx}]
                        (reduce (fn [w idx]
                                  (update-in w [idx] nil-inc))
                                w (get-fv gold head modifier)))
                      weight gold)]
        (reduce (fn [w {head :head, modifier :idx}]
                  (reduce (fn [w idx]
                            (let [w (update-in w [idx] nil-dec)]
                              (if (zero? (get w idx))
                                (dissoc w idx)
                                w)))
                          w (get-fv prediction head modifier)))
                w prediction)))))
