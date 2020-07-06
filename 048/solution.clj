(defn self-power-mod
  [m n]
  (reduce (fn [r x] (mod (* r x) m)) (repeat n n))) 


(defn sum-self-power-series-mod
  [n m]
  (mod (->> (range 1 (inc n))
         (map bigint)
         (map (partial self-power-mod m))
         (apply +)) m))

(println (sum-self-power-series-mod 1000 (bigint (Math/pow 10 10))))
