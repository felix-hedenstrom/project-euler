(defn find-divisors
  [n]
  (loop [i 1
         divisors '()]
    (cond
      (< (/ n 2) i)
        divisors
      (= (rem n i) 0)
        (recur (+ 1 i) (conj divisors i))
      :else
        (recur (+ 1 i) divisors))))


(assert (= '(5 2 1) (find-divisors 10)))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn coprime?
  [a b]
  (= (gcd a b) 1))

(defn sieve
  "Get all the primes up to n"
  [n]
  (loop [sieve (apply sorted-set (range 2 (+ n 1)))
         i 2
         j 2]
    (let [k (* i j)]
      (cond 
        (< (Math/sqrt n) i) 
          (vec sieve)
        (< n k)
          (recur sieve (+ i 1) 2)
        :else
          (recur 
            (disj sieve k)
            i
            (+ j 1))))))

(assert (= [2 3 5 7] (sieve 10)))
