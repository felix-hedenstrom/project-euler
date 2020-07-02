(load-file "../util.clj")

(defn palindrome-in-base?
  [base n] 
  (is-palindrome? (Integer/toString n base)))

(assert (palindrome-in-base? 10 585))
(assert (palindrome-in-base? 2 585))

(defn double-palindrome
  [n base1 base2]
    (->> (range 1 n)
         (filter (partial palindrome-in-base? base1))
         (filter (partial palindrome-in-base? base2))
         (apply +)))

(println (double-palindrome 1000000 10 2))
