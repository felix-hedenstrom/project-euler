(require '[clojure.string :as str])

(load-file "../util.clj")

(def file (-> (slurp "p059_cipher.txt")
              (str/split #",")))


(defn decrypt
  [encrypted-text decrypt-key]
  (map (fn [ec dk]
         (bit-xor ec dk)) encrypted-text (cycle decrypt-key)))

(assert (= '(65) (decrypt '(107) '(42))))

(defn chars-to-text
  [chars-text]
  (apply str (map char chars-text))) 


(defn english-text?
  [text]
  ; Most common trigrams
  (and (str/includes? text "the")
       (str/includes? text "and")
       (str/includes? text "ing")))

(def lower-case-start 97)
(def lower-case-end 122)

(defn find-decryption
  [path]
  (let [encrypted-chars (as-> (slurp path) $
                          (str/split $ #",")
                          (map read-string $))
        lower-case-range (range lower-case-start (inc lower-case-end))
        possible-keys (->> (cartesian-product list lower-case-range lower-case-range) 
                           (reduce concat)
                           (cartesian-product cons lower-case-range)
                           (reduce concat))

        correct-key (-> (filter 
                          (fn [k] 
                            (-> (decrypt encrypted-chars k)
                                chars-to-text
                                english-text?))
                          possible-keys)
                        first)
        correct-text (-> (decrypt encrypted-chars correct-key)
                          chars-to-text)]
    {:correct-key correct-key
     :correct-text correct-text
     :char-sum (apply + (decrypt encrypted-chars correct-key))}))


(println (find-decryption "p059_cipher.txt"))
