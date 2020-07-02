; a^2 + b^2 = c^2
; -> b = \sqrt(c^2 - a^2) = b(c,a)
;
; a + b + c = p
; c = p - a - b
; substitute b with b(c,a)
; c = p - a - \sqrt(c^2 - a^2)
; (c + a - p) ^2 = c^2 - a^2
; => c = (2ap - 2a^2 -p^2)/(2a - 2p) 

(defn c-func
  [p a]
  (/ 
    (- (* 2 a p) (* 2 a a) (* p p))
    (- (* 2 a) (* 2 p))))

(defn b-func 
  [a c]
  (int (Math/sqrt (- (* c c) (* a a)))))


(let [p 120
      a 20
      c (c-func p a)
      b (b-func a c) ]
  (assert (= 52 c))
  (assert (= 48 b)))


(defn solutions-for-p
  [p]
  (->> (range 1 p)
       (map #(hash-map :a % :b (b-func % (c-func p %)) :c (c-func p %)))
       (filter 
          (fn [{a :a b :b c :c}] 
            (and (< a b)
              (= p (+ a b c)))))
       (filter #(integer? (get % :c)))
       (count)))

(assert (solutions-for-p 3))

(defn best-perimiter
  [max-p]
  (->> (inc max-p)
       (range 1)
       (map (fn [p] [(solutions-for-p p) p]));))
       (apply (partial max-key first))
       (second)))

(println (best-perimiter 1000))
       

