(load-file "../hugeint.clj")

(defn first-fib-with-n-digits
  [n]
  (loop [i 2
         a (hugeint 0)
         b (hugeint 1)]
    (let [next-number (add a b)]
      (if (<= n (count (get next-number :digits))) 
        i
        (recur 
          (inc i)
          b
          next-number)))))

(assert (= 12 (first-fib-with-n-digits 3)))
(println (first-fib-with-n-digits 1000))
