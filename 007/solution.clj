(load-file "../util.clj")

(assert (= 13 (get (sieve 20) 5)))

(println (get (sieve 1000000) 10000))
