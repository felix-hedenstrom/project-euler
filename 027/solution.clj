(load-file "../util-prime.clj")

(defn quadratic-formula
  [a b n]
  (+ (* n n) (* n a) b))

(defn consecutive-primes
  [a b]
  (loop [n 0]
    (if (is-prime? (quadratic-formula a b n))
      (recur (inc n))
      (dec n))))

(assert (= (consecutive-primes -79 1601) 79))
(assert (= (consecutive-primes 1 41) 39))

(defn longest-consecutive-primes
  [amax bmax]
  (->> (cartesian-product list (range (- amax) amax) (range (- (inc bmax)) (inc bmax)))
       (apply concat)
       (map (fn [[a b]] [(consecutive-primes a b) (* a b) a b]))
       (apply (partial max-key first)))) 

(println (longest-consecutive-primes 1000 1000))
