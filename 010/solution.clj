(load-file "../util.clj")

(defn prime-sum
  [n]
  (apply + (sieve n)))

(assert (prime-sum 10))

(println (prime-sum 2000000))
