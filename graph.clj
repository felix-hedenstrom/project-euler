(defn create-graph 
  [& {nodes :nodes edges :edges}] 
  {:nodes (set nodes) :edges edges})

(defn add-edge
  ([graph from to weight]
   (-> (assoc-in graph [:edges from to] weight)
       (update :nodes #(conj % from))
       (update :nodes #(conj % to))))
  ([graph from to]
   (add-edge graph from to nil)))

(defn add-double-edge
  ([graph from to]
   (add-double-edge graph from to nil))
  ([graph from to weight]
   (-> (add-edge graph from to weight)
       (add-edge to from weight))))

(defn get-nodes
  [graph]
  (get graph :nodes))

(defn get-neighbors
  ([graph]
   (get graph :edges))
  ([graph node]
   (-> (get-neighbors graph) 
       (get node)
       (keys))))

(assert (= '(:a) (-> (create-graph)
                     (add-double-edge :a :b 0)
                     (get-neighbors :b))))

(defn get-distance
  [graph from to]
  (get-in graph [:edges from to]))


(defn dijkstra
  ([graph source destination]
   (let [[_ previous] (dijkstra graph source)]
     (loop [path '()
            v destination]
       (if (nil? v)
         path
           (recur
             (conj path v) 
             (get previous v))))))
  ([graph source]
   (let [min-distance-in-queue (fn [queue distance]
                                 (apply (partial min-key #(get distance %)) queue))
         dijkstra-inner (fn [graph u distance previous]
                          (loop [neighbors (get-neighbors graph u)
                                 distance distance
                                 previous previous]
                            (if (empty? neighbors)
                              [distance previous]
                              (let [v (first neighbors)
                                    alt (+ (get distance u) (get-distance graph u v))]

                                (if (< alt (get distance v)) 
                                  ; Faster to go through u
                                  (recur 
                                    (rest neighbors)
                                    (assoc distance v alt)
                                    (assoc previous v u))
                                  ; Not faster, continoue
                                  (recur 
                                    (rest neighbors)
                                    distance
                                    previous))))))]

     (loop [distance (-> (into {} (map (fn [v] [v Integer/MAX_VALUE]) (get-nodes graph)))
                         (assoc source 0))
            previous (into {} (map (fn [v] [v nil]) (get-nodes graph)))
            q (into (set {}) (get-nodes graph))]
       (if (empty? q)
         [distance 
          previous]
         (let [u (min-distance-in-queue q distance)
               [distance previous] (dijkstra-inner graph u distance previous)]
           (recur distance previous (disj q u))))))))


(let [g (-> (create-graph :nodes [1 2 3 4 5 6])
            (add-double-edge 1 2 7)
            (add-double-edge 1 3 9)
            (add-double-edge 1 6 14)
            (add-double-edge 2 3 10)
            (add-double-edge 2 4 15)
            (add-double-edge 3 6 2)
            (add-double-edge 3 4 11)
            (add-double-edge 4 5 6)
            (add-double-edge 5 6 9))
      [distance previous] (dijkstra g 1)]
  (assert (= 9 (get distance 3)))
  (assert (= 11 (get distance 6))))


