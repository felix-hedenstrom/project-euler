(defn hugeint
  [n]
  {:digits
   (->> (str n) 
        seq
        (map int)
        (map #(- % 48))
        reverse
        vec)})

(defn to-int
  [{digits :digits}]
  (->> (range 0 (count digits))
       (map 
         (fn 
           [i] 
           (* (nth digits i) (Math/pow 10 i))))
       (apply +)
       int))

(assert (= 500 (to-int (hugeint "500"))))
(assert (= 10 (to-int (hugeint 10)))) 
(assert (= 6 (to-int (hugeint 6)))) 
(assert (= 0 (to-int (hugeint 0)))) 


(defn add
  ; Only works for positive numbers so far
  ([{digits1 :digits} {digits2 :digits}]
  (loop [digits1 digits1
         digits2 digits2 
         answer []
         remainder 0]
    (if (and (empty? digits1) (empty? digits2))
      {:digits 
       (if (zero? remainder)
         answer
         (conj answer remainder))}
      (let [d1 (or (first digits1) 0)
            d2 (or (first digits2) 0)
            x (+ d1 d2 remainder)
            remainder (if (< x 10) 0 (quot x 10))
            ans (mod x 10)]
        (recur (rest digits1) (rest digits2) (conj answer ans) remainder))))) 
  ([hi1 hi2 & himore]
    (reduce (fn [sum x] (add sum x)) (add hi1 hi2) himore)))

(assert (= 6 (to-int (add (hugeint 2) (hugeint 4)))))
(assert (= 10 (to-int (add (hugeint 5) (hugeint 5)))))
(assert (= 10 (to-int (add (hugeint 10) (hugeint 0)))))
(assert (= 500 (to-int (add (hugeint 1) (hugeint 499)))))
(assert (= 3 (to-int (add (hugeint 1) (hugeint 1) (hugeint 1)))))

(defn mult
  [{digits1 :digits} {digits2 :digits}]

  (let [digits1 digits1
        digits2 digits2
        p (count digits1)
        q (count digits2)]
    (loop [a (range 0 p)
           b (range 0 q)
           product (vec (repeat (dec (+ p q)) 0))
           carry 0] 
      ;(println "a: " (map (partial nth digits1) a))
      ;(println "b: " (map (partial nth digits2) b))
      ;(println "product: " product)
      ;(println "carry: " carry)
      (cond
        (empty? b)
        {:digits (->> (update product (+ (dec p) (dec q)) (partial + carry))
                      (reverse)
                      (drop-while zero?)
                      (reverse)
                      vec)}
        (empty? a)
        (recur 
          (range 0 p) 
          (rest b) 
          (assoc product (+ (first b) p) carry);(partial + carry)) 
          0)
        :else 
        (let [ai (first a)
              bi (first b)
              i (+ ai bi)
              n (+ (nth product i) carry (* (nth digits1 ai) (nth digits2 bi)))
              carry (quot n 10)]
          (recur 
            (rest a) 
            b
            (assoc product i (mod n 10)) 
            carry))))))


(assert (= 6 (to-int (mult (hugeint 1) (hugeint 6)))))
(assert (= 100 (to-int (mult (hugeint 10) (hugeint 10)))))
(assert (= 32 (to-int (mult (hugeint 2) (hugeint 16)))))
(assert (= 9801 (to-int (mult (hugeint 99) (hugeint 99)))))


(defn factorial
  [n]
  (loop [i 2
         ans (hugeint 1)]
    (if (< n i)
      ans
      (recur (inc i) (mult ans (hugeint i)))))) 


(assert (= 1 (to-int (factorial 1))))
(assert (= 2 (to-int (factorial 2))))
(assert (= 6 (to-int (factorial 3))))
(assert (= 3628800 (to-int (factorial 10))))
