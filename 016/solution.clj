(load-file "../hugeint.clj")


(defn power-digit-sum
  [base n]
  (let [hibase (hugeint base)]
    (as-> 
      (reduce 
        (fn 
          [x i]
          (mult x hibase)) hibase (range 0 (dec n))) $ 
      ;)))
      (get $ :digits)
      (apply + $))))

(assert (= 26 (power-digit-sum 2 15))) 
(println (power-digit-sum 2 1000)) 

