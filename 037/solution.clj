(load-file "../util.clj")
(require 'clojure.set)

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

(defn find-n-truncatable-primes
  [n]
  (loop [found '()
         base-candidates (flatten (cartesian-product str valid-digits valid-digits))]
    (if (<= n (count found))
      found
      (let [lt (set (filter #(partially-truncatable-prime? % rest) base-candidates)) 
            rt (set (filter #(partially-truncatable-prime? % drop-last) base-candidates))]
        (recur 
          (concat found (clojure.set/intersection lt rt))
          (flatten (cartesian-product str valid-digits (apply list (clojure.set/union rt lt)))))))))

(println (apply + (map read-string (find-n-truncatable-primes 11))))

          

          
       
