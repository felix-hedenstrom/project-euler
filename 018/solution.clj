(require '[clojure.string :as str])

(defn maximum-path-sum	
  [file-path]	
  (let [lines (->> (slurp file-path)
                   (str/split-lines)
                   (map #(str/split % #" "))
                   (map #(map (fn [n] (Integer/parseInt n)) %))
                   (reverse))
        base-data (map (fn [n] {:node-value n :highest-sum n}) (first lines))
        lines (rest lines)]
    (loop [previous-row base-data
           row (first lines)
           remaining-lines (rest lines)]
      (if (nil? row)
        (get (first previous-row) :highest-sum)
        (recur 
          (->> (range 0 (count row))
               (map 
                 (fn 
                   [i] 
                   (let [node-value (nth row i)
                         down-left (get (nth previous-row i) :highest-sum)
                         down-right (get (nth previous-row (inc i)) :highest-sum)]
                     {
                      :node-value node-value 
                      :highest-sum (+ 
                                     node-value 
                                     (max 
                                       down-left
                                       down-right))}))))
          (first remaining-lines) 
          (rest remaining-lines)))))) 

(assert (= 23 (maximum-path-sum "easy.txt")))
(println "018: " (maximum-path-sum "018.txt"))
(println "067: " (maximum-path-sum "067.txt"))
