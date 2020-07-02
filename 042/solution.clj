(defn t-to-n
  [t]
  (- (Math/sqrt (+ (* 2 t) (/ 1 4))) (/ 1 2)))

(assert (= 10.0 (t-to-n 55)))

(defn triangle-number?
  [t]
  (-> (rem (t-to-n t) 1)
      (zero?)))

(assert (triangle-number? 1))
(assert (triangle-number? 3))
(assert (triangle-number? 6))
(assert (triangle-number? 10))
(assert (triangle-number? 15))
(assert (triangle-number? 21))
(assert (triangle-number? 28))
(assert (triangle-number? 36))
(assert (triangle-number? 45))
(assert (triangle-number? 55))

(assert (not (triangle-number? 2)))
(assert (not (triangle-number? 50)))


(def file (slurp "words.txt"))

(def words (map #(apply str (rest (drop-last %))) (clojure.string/split file #",")))

(defn word-score
  [w]
  (->> (map (fn [n] (- (int n) 64)) (clojure.string/upper-case w))
       (apply +)))

(assert (= 55 (word-score "SKY"))) 

(println (->> (map word-score words)
              ;(count)
              ;))
              (filter triangle-number?)
              (count)) 
