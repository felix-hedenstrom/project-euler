(defn find-divisors
  [n]
  (let [f (fn
            [k]
            (zero? (rem n k)))]
    (->> (range 1 (inc n))
         (filter f))))


(assert (= '(1 2 5 10) (find-divisors 10)))

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

(defn mod-zero?
  [a b]
  (= (rem a b) 0))

(assert (mod-zero? 4 2))
(assert (not (mod-zero? 4 3)))

(defn cartesian-product
   [f a b]
   (map 
    (fn 
    [a0]
          (map (partial f a0) b))
    a))

(defn is-palindrome?
  [s]
  (loop [s s]
    (cond
      (<= (count s) 1)
        true
      (= (first s) (last s))
        (recur (subs s 1 (- (count s) 1)))
      :else
        false)))

(assert (is-palindrome? "1"))
(assert (is-palindrome? "abba"))
(assert (is-palindrome? "abcba"))
(assert (is-palindrome? "585"))
(assert (not (is-palindrome? "test")))
