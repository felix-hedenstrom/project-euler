; Pythagorean triplets are always on the form a = m^2 - n^2, b = 2mn, c = m^2 + n^2
; this implies: 
;   a + b + c = m^2 - n^2 + 2mn + m^2 + n^2 = 2m^2 + 2mn 
; since we are looking for a + b + c = 1000 we can also say
; 2m^2 + 2mn = 1000 => m^2 + mn = 500 => m(m + n) = 500 

(def target 500)

; We know that m must divide 500, so we will check its divisors
(load-file "../util.clj")
(def m-candidates (find-divisors target))

; m(m + n) = 500 =>
; n = 500/m - m
(defn n-from-m
  [m]
  (- (/ target m) m))

(defn both-odd
  [a b]
  (and (odd? a) (odd? b)))

(defn get-a
  [n m]
  (- (* m m) (* n n)))

(defn get-b 
  [n m]
  (* 2 m n))

(defn get-c 
  [n m]
  (+ (* m m) (* n n)))

(def m
  (loop [m-cand m-candidates]
    (if-let [m (first m-cand)]
      (let [n (n-from-m m)]
        (cond 

          (or 
            (> 1 (get-a n m))
            (> 1 (get-b n m))
            (> 1 (get-c n m))) 
            (recur (rest m-cand))
          (= (* m (+ m (n-from-m m))) target)
            m 
          :else 
            (recur (rest m-cand))))
      nil)))

(def n (n-from-m m))

(def a (- (* m m) (* n n)))
(def b (* 2 m n))
(def c (+ (* m m) (* n n)))

(println a b c)
(println (- (+ (* a a) (* b b)) (* c c)))
(println (+ a b c))

(println (* a b c))
