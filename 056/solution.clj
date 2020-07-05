(load-file "../util.clj")
(load-file "../hugeint.clj")

(defn largest-digit-sum
  [a b]
  (->> (cartesian-product list (range 1 a) (range 1 b))
       (apply concat)
       (map (partial apply pow))
       (map :digits)
       (map (partial apply +))
       (apply max)))


(println (largest-digit-sum 100 100))

