(load-file "../util.clj") 

(defn abundant-numbers
  ; Find all numbers < n that are abundant
  [n]
  (->> (range n)
       (filter (fn [n] 
                 (< n (apply + (drop-last (find-divisors n))))))
       (apply sorted-set)))

(assert (= (sorted-set 12) (abundant-numbers 13)))

(defn sum-of-set?
  [s n]
  (some (fn [x] (contains? s (- n x))) s)) 

(assert (sum-of-set? (sorted-set 12) 24)) 
(assert (not (sum-of-set? (sorted-set 12) 23))) 

(def abundant-limit 28123) ; By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. 

(defn cant-be-written-as-sum-of-abundant
  ; Find all x < n where x can't be expressed as the sum of two abundant numbers
  ([n limit]
    (let [an (abundant-numbers (inc limit))]
      (->> (range (inc limit))
           (filter #(not (sum-of-set? an %))))))
  ([n]
   (cant-be-written-as-sum-of-abundant n abundant-limit)))


(println (apply + (cant-be-written-as-sum-of-abundant abundant-limit)))
