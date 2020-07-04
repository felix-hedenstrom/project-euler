(load-file "../hugeint.clj")

(defn digit-sum 
  [hi]
  (apply + (:digits hi)))

(assert (= 27 (digit-sum (factorial 10)))) 
(println (digit-sum (factorial 100)))
