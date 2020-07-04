(defn routes-in-nxm
  ([n m]
   (first (routes-in-nxm n m {})))
  ([n m history]
  (assert (<= 0 n))
  (assert (<= 0 m))
  (if-let [x (get-in history [n m])]
    [x history]
    ; We did not have the answer
    (let [[x history]
          (cond 
            (and (zero? n) (zero? m))
              [1 history] 
            (zero? n)
              (routes-in-nxm n (dec m) history)
            (zero? m) 
              (routes-in-nxm (dec n) m history)
            :else
              (let [[x history] (routes-in-nxm (dec n) m history)
                    [y history] (routes-in-nxm n (dec m) history)]
                [(+ x y) history]))]
      [x (assoc-in history [n m] x)]))))

(assert (= 2 (routes-in-nxm 1 1)))
(assert (= 6 (routes-in-nxm 2 2)))

(println (routes-in-nxm 20 20))
