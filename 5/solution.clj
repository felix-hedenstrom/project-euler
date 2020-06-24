(defn divisible-by?
  [n numbers]
  (empty? 
      (filter 
          #(not 
            (zero? 
              (mod n %))) 
          numbers)))

(assert (divisible-by? 2520 (range 1 11)))
(assert (not (divisible-by? 2520 (range 1 12))))

(println
  (divisible-by? (* 2 3 2 5 7 2 3 11 13 2 17 19) (range 1 21))
  (* 2 3 2 5 7 2 3 11 13 2 17 19))

