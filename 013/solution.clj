(require '[clojure.string :as str])
(load-file "../hugeint.clj")

(defn first-n-digits-of-sum
  [file-path n]
  (->> (slurp file-path)
       (str/split-lines)
       (map hugeint)
       (apply add)
       (:digits)
       (reverse)
       (take n)
       (apply str)))

(println (first-n-digits-of-sum "50-digit-numbers.txt" 10))

