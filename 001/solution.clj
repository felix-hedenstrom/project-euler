(defn is-multiple?
  [n k]
  (zero? (mod n k)))

(defn candidates
  [n]
  (filter #(or (is-multiple? % 3) (is-multiple? % 5)) (range 1 n)))

(defn modsum
  [n]
  (apply + (candidates n)))

(assert (= 23 (modsum 10)))
(println (modsum 1000))
