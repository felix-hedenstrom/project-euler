(defn sum-of-even-fib
  [n]
  (loop [checked 0
         sum 0
         a 0
         b 1]

    (if (>= b n)
      sum
      (let [next-number (+ a b)]
        (recur 
          (+ 1 checked)
          (if (even? next-number)
            (+ sum next-number) 
            sum)
          b
          (+ a b))))))

(println (sum-of-even-fib 10))
(println (sum-of-even-fib 4000000))
