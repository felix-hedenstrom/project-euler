(def coins '(1 2 5 10 20 50 100 200))

(defn ways-to-make
  [n coins]
  (let [coins (reverse (sort coins))
        c (first coins)]
    (if (empty? coins)
      nil
      (->>  (take-while #(<= (* c %) n) (iterate inc 0))
            (map 
              (fn 
                [k] 
                (let [made (* c k)
                      combination {:coin c :amount k}]
                  (if (= made n)
                    (list (list combination))
                    (if-let [wtm (doall (ways-to-make (- n (* c k)) (rest coins)))]
                      (map #(conj % combination) wtm)
                      nil))))) 
            (filter #(not (nil? %)))
            (apply concat)))))
    

(println (count (ways-to-make 200 coins)))
