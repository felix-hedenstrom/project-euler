(load-file "../util.clj")

(defn avg 
  [a b]
  (/ (+ a b) 2))

(defn nth-triangle
  [n]
  (-> (+ n 1)
      (* n)
      (/ 2)))

(defn number-of-divisors 
  [n]
  (count (find-divisors n)))

(defn first-triangle-number-with-over-k-divisors
  [n]
  (loop [i 1
         largest 0]
    (let [nod (if (even? i) (* (number-of-divisors (/ i 2)) (number-of-divisors (+ i 1))) (* (number-of-divisors i) (number-of-divisors (/ (+ i 1) 2))))]
      (if (< n nod) 
        (nth-triangle i) 
        (recur 
          (inc i) 
          (if (< largest nod)
            (do 
              nod)
            largest))))))

    
(assert (= 28 (first-triangle-number-with-over-k-divisors 5)))

    
(println (first-triangle-number-with-over-k-divisors 500))

