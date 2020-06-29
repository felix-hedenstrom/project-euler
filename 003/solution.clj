(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn pollard-rho
  [n]
  (let [f (fn [x n]
            (-> (* x x)
                (inc)
                (mod n)))
        g #(f % n)]
    (loop [x (bigint 1)
           y (bigint 2) 
           d (bigint 1)]
      (if (not= d 1)
        d
        (recur 
          (g x)
          (g (g y))
          (gcd (Math/abs (long (- x y))) n))))))

(defn all-factors
  [n]
  (loop [n n
         factors '()]
    (if (= n 1)
      factors
      (if-let [factor (pollard-rho n)]
          (recur 
            (/ n factor)
            (conj factors factor))
          factors))))
