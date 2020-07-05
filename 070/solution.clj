(load-file "../util.clj")

(defn euler-phi
  [n]
  (loop [n n]
    (if (= n 1)
      1
      (let [x (mod n 2)]
        (if (zero? x) 
          (if (even? (/ n 2))
            (* 2 (euler-phi (/ n 2)))
            (recur (/ n 2)))
          (->> (range 1 n) 
               (filter (partial coprime? n))
               (count)))))))

(defn remove-all-factors-of
  [n f]
  (loop [n n]
    (if (zero? (mod n f))
      (recur (/ n f))
      n)))


(defn phi
  [n]
  (loop [n n
         result n
         i 2]
    (cond
      (not (<= (* i i) n))
      (if (> n 1)
        (- result (/ result n))
        result)
      (zero? (mod n i))
      (recur (remove-all-factors-of n i) (- result (/ result i)) (inc i)) 
      :else
      (recur n result (inc i)))))


(defn euler-phi-upper-bound
  [n]
  (int (* (Math/pow Math/E (- 0.577215)) (/ n (Math/log (Math/log n))))))

(time (assert (= 79180 (euler-phi 87109))))
(assert (= 1 (euler-phi 1)))
(assert (= 6 (euler-phi 9)))

(time (assert (= 79180 (phi 87109))))
(assert (= 1 (phi 1)))
(assert (= 6 (phi 9)))

(defn permutation?
  [a b]
  (and 
    ;(= (count (str a)) (count (str b)))
    (= (frequencies (str a)) (frequencies (str b)))))

(assert (permutation? 79180 87109))

(defn totient-permutation
  [n]
  (->> (range 2 n)
       ;(map #(count (str (euler-phi-upper-bound %))))
       ; 
       ;(count)))
       ;))
       ;(map (fn [x] [x (euler-phi x)]))
       ;(filter (partial apply permutation?))))
       (reduce 
         (fn 
           [best c]
           (let [{ratio :ratio} best
                 upper-bound (int (/ c ratio))] ; If the ratio c/chi is as good as our best, chi will be this number 
             (if (< (count (str upper-bound)) (count (str c)))
               best
               (let [p (phi c)
                     c-ratio (/ c p)]
                 (if (and (permutation? c p) (< c-ratio ratio))
                   (do 
                     (println best)
                     {:n c :phi p :ratio c-ratio}) 
                   best)))))
         {:ratio 2})))

(defn totient-permutation2
  [n]
  (->> (range 2 n)
       (map (fn [x] [x (phi x)]))
       (filter (partial apply permutation?))
       (apply (partial min-key (fn [[x p]] (/ x p))))))
       ;(min-key (fn [[x p]] (/ x p)))))
       
;(time (println (totient-permutation 100000)))
(time (println (totient-permutation2 10000000)))
