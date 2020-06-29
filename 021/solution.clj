(load-file "../util.clj")

(defn divisor-sum
  [n]
  (- (apply + (find-divisors n)) n)) ; find-divisors also finds the number itself: n, so find-divisors = find-proper-divisors + n 

(assert (= 284 (divisor-sum 220)))
(assert (= 220 (divisor-sum 284)))

(defn generate-mapping
  [n]
  (vec (map divisor-sum (range 0 n))))


(assert (= 284 (nth (generate-mapping 500) 220)))

(defn all-amicable
  [n]
  (let [mapping (generate-mapping n)]
      (filter 
        (fn 
          [n] 
          (let [a n
                b (nth mapping a nil)
                db (nth mapping b nil)]
            (and (= a db) (not= a b))))
        (range 1 n))))

(println (apply + (all-amicable 10000)))
