(defn square
  [n]
  (* n n))

(defn sum-of-squares
  [r]
  (apply + (map square r))) 

(defn square-of-sums
  [r]
  (square (apply + r)))

(defn solution
  [n]
  (let [r (range 1 (+ n 1))]
    (- (square-of-sums r) (sum-of-squares r)))) 

(assert (= 2640 (solution 10)))

(println (solution 100))
