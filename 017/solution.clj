(load-file "../hugeint.clj")

(def base-numbers 
  {0 ""
   1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"
   10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"
   20 "twenty"
   30 "thirty"
   40 "forty"
   50 "fifty"
   60 "sixty"
   70 "seventy"
   80 "eighty"
   90 "ninety"})

(defn index-translate
  [i d]
  (if (or (nil? d) (zero? d))
    nil 
   (cond 
     (= i 3)
     (str (get base-numbers d) " thousand")
     (= i 2)
     (str (get base-numbers d) " hundred")
     (= i 1)
     (str (get base-numbers (* 10 d)))))) 

(assert (= "twenty" (index-translate 1 2)))

(defn to-string
  [n]
  (if (contains? base-numbers n)
    (get base-numbers n) 
    (let [digits (:digits (hugeint n))
          thousands (index-translate 3 (get digits 3)) 
          hundreds (index-translate 2 (get digits 2))
          last-two (if-let [two-first (get base-numbers (mod n 100))]
                     two-first
                     (str (index-translate 1 (get digits 1)) "-" (get base-numbers (get digits 0))))
          last-two (if (= "" last-two)
                     nil
                     last-two)]
      (str 
        thousands 
        (if (and thousands hundreds)
          " and "
          nil)
        hundreds 
        (if (and (or thousands hundreds) last-two)
          " and "
          nil)
        last-two))))

;(println (to-string 1000))

(assert (= (to-string 30) "thirty"))
(assert (= (to-string 35) "thirty-five"))
(assert (= (to-string 342) "three hundred and forty-two"))
(assert (= (to-string 115) "one hundred and fifteen"))
(assert (= (to-string 1000) "one thousand"))


(defn word-length-special 
  [w]
  (-> (remove #(or (= % \space) (= % \-)) w)
      (count)))

;(println (word-length-special "one hundred and fifteen")) 

(assert (= 23 (word-length-special "three hundred and forty-two")))
(assert (= 20 (word-length-special "one hundred and fifteen")))

(println (->> (range 1001) 
              (map to-string) 
              (map word-length-special)
              (apply +)))
