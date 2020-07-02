(load-file "../util.clj")
(defn is-prime?
  [n]
  (cond 
    (<= n 1)
      false
    (= n 2)
      true
    :else
      (->> (range 2 (inc (Math/sqrt n)))
           (filter (partial mod-zero? n))
           (count)
           (zero?))))

(assert (is-prime? 7))
(assert (is-prime? 11))
(assert (not (is-prime? 4)))
(assert (not (is-prime? -14)))
(assert (is-prime? 2))
(assert (not (is-prime? 1)))

(defn partially-truncatable-prime?
  [n f]
  (if (= (count (str n)) 1)
    false
    (loop [s (str n)]
      (cond 
        (empty? s)
          true
        (not (is-prime? (read-string s)))
          false
        :else
          (recur (apply str (f s)))))))

(assert (partially-truncatable-prime? 3797 rest))
(assert (partially-truncatable-prime? 23 rest))
(assert (not (partially-truncatable-prime? 7 rest)))
(assert (not (partially-truncatable-prime? 24 rest)))

(assert (partially-truncatable-prime? 3797 drop-last))
(assert (partially-truncatable-prime? 23 drop-last))
(assert (not (partially-truncatable-prime? 7 drop-last)))
(assert (not (partially-truncatable-prime? 24 drop-last)))

(defn is-truncatable-prime?
  [n]
  (and (partially-truncatable-prime? n rest) (partially-truncatable-prime? n drop-last)))

(assert (is-truncatable-prime? 3797))
(assert (is-truncatable-prime? 23))

(def valid-digits [1 2 3 5 7 9])

(defn cartesian-product
   [f a b]
   (map 
    (fn 
    [a0]
          (map (partial f a0) b))
    a))
