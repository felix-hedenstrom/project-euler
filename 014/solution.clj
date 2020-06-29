(defn collatz-length
  ([n]
    (collatz-length n {}))
  ([n history]
    (if (= 1 n)
      [1 history]
      (if-let [h (get history n)]
        [h history]
        (let [iter (if (even? n)
                     (/ n 2)
                     (+ (* 3 n) 1))
              [iterans history] (collatz-length iter history)
              ans (+ 1 iterans)]
          [ans (assoc history iter ans)])))))  
          
          
(assert (= 10 (first (collatz-length 13))))

(defn longest-collatz
  [n]
  (loop [i 1
         longest [0 0]
         history {}]
    (if (< n i)
      longest 
      (let [[ans history] (collatz-length i history)]
        (recur
          (inc i)
          (if (< (last longest) ans)
            [i ans]
            longest)
          history)))))
        
(println (longest-collatz 1000000))

