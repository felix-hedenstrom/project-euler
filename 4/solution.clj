(defn is-palindrome?
  [s]
  (loop [s s]
    (cond
      (<= (count s) 1)
        true
      (= (first s) (last s))
        (recur (subs s 1 (- (count s) 1)))
      :else
        false)))

(assert (is-palindrome? "abba"))
(assert (is-palindrome? "abcba"))
(assert (not (is-palindrome? "test")))


(defn find-largest-palindrome-product
  ; Find largest product x * y = z, where z is a palindrome and
  ; a <= x, y <= b
  [a b]
  (loop [i b
         j b
         palindromes '()]
    (cond
      (< j a)
        (recur (- i 1) i palindromes) 
      (< i a)
        palindromes
      (is-palindrome? (str (* i j)))
        (recur i (- j 1) (conj palindromes (* i j)))
      :else
        (recur i (- j 1) palindromes)))) 

(assert (= 9009 (apply max (find-largest-palindrome-product 10 99))))
(println (apply max (find-largest-palindrome-product 100 999)))

