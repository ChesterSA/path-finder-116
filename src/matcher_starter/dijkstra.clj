(ns matcher-starter.dijkstra
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(require '[matcher-starter.priority-map :refer [priority-map priority-map-keyfn]])

; '''Modification of Djikstra''' [[Media:https://www.ummels.de/2014/06/08/dijkstra-in-clojure/]]
(use 'org.clojars.cognesence.matcher.core)

(def numbers '(3 4 1 2 8 6
                6 1 8 2 7 4
                5 9 3 9 9 5
                8 4 1 3 2 6
                3 7 2 8 6 4
                ))

(def smallnum '(1 2 3 4 5 6 7 8 9))

(defn make-matrix [row col numbers]
  (cond
    (= 0 row)
    nil
    :else
    (cons (take col numbers)
          (make-matrix (- row 1) col (drop col numbers)))))

(defn make-matrix2 [col numbers]
  (partition col numbers))

(def matrix (make-matrix 5 6 numbers))
(def small (make-matrix 3 3 smallnum))

(defn get-at [matrix x y]
  (nth (nth matrix y) x))

(defn get-id [x y]
  (str x "-" y))

(defn make-edge [matrix start finish]
  (let [start-x (first start)
        start-y (second start)
        finish-x (first finish)
        finish-y (second finish)]
    [(get-id start-x start-y) (get-id finish-x finish-y) (get-at matrix finish-x finish-y)]))

(defn move-up-right [matrix x y]
  (if (= y 0)
    (list (+ x 1) (- (count matrix) 1))
    (list  (+ x 1)  (- y 1)) )
  )

(defn move-right [x y]
  (list (+ x 1) y))

(defn move-down-right [matrix x y]
  (if (= y (- (count matrix) 1))
    (list (+ x 1) 0)
    (list(+ x 1) (+ y 1)))
  )

(defn make-three-edges [matrix x y]
  (list (make-edge matrix (list x y) (move-up-right matrix x y))
   (make-edge matrix (list x y) (move-right x y))
   (make-edge matrix (list x y) (move-down-right matrix x y))))

(defn make-node-list
  [x y finalx finaly]
    (cond
      (and (= x finalx) (= y finaly))
        (get-id x y)
      (= x finalx)
        (list (get-id x y) (make-node-list 0 (inc y) finalx finaly))
      :else
        (list (get-id x y) (make-node-list (inc x) y finalx finaly))))

(defn make-nodes [matrix]
  (into [] (flatten (make-node-list 0 0 (dec (count (first matrix))) (dec (count matrix))))))

(defn make-edge-list
  [matrix x y finalx finaly]
  (cond
    (and (= x finalx) (= y finaly))
      (make-three-edges matrix x y)
    (= x finalx)
      (list (make-edge matrix (list x y) (move-up-right matrix x y)) (make-edge matrix (list x y) (move-right x y)) (make-edge matrix (list x y) (move-down-right matrix x y)) (make-edge-list matrix 0 (inc y) finalx finaly))
    :else
      (list (make-edge matrix (list x y) (move-up-right matrix x y)) (make-edge matrix (list x y) (move-right x y)) (make-edge matrix (list x y) (move-down-right matrix x y)) (make-edge-list matrix (inc x) y finalx finaly))))

(defn make-edges [matrix]
  (partition 3 (flatten (make-edge-list matrix 0 0 (- (count (first matrix)) 2) (dec (count matrix))))))



(defn map-vals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn min2 [v1 v2]
  (if (< (first v1) (first v2))
    v1
    v2))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (graph n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start graph]
  (loop [q (priority-map-keyfn first start [0 nil]),
         r {}]
    (if-let [[v [d u]] (peek q)]
      (let [dist (-> (graph v)
                     (remove-keys r)
                     (map-vals (fn [cost] (let [new-cost (+ d cost)] [new-cost v]))))]
        (recur (merge-with min2 (pop q) dist) (assoc r v [d u])))
      r)))

(defn make-adj-list
  "Convert a list of nodes and a list of edges into an
  adjacency list structure.  For example: with N [1 2 3],
  E [[1 2] [2 3] [1 3]], the result is {1 [2 3], 2 [3], 3 []}"
  [Nodes Edges]
  (let [init-graph (reduce merge (map #(hash-map %1 {}) Nodes))]
    (reduce #(merge-with merge %1 %2)
            init-graph
            (map #(hash-map (nth % 0) (hash-map (nth % 1) (nth % 2))) Edges))))

(defn path-to [goal dik]
  (if (contains? dik goal)
    (reverse (take-while identity (iterate (comp second dik) goal)))
    nil))

(defn cost-to [goal dik]
  (if (contains? dik goal)
    (first (dik goal))
    -1))

(defn path-finder [matrix start dest]
  (let [graph (make-adj-list (make-nodes matrix) (make-edges matrix))
        dij (dijkstra start graph)]
    {:cost (cost-to dest dij)
     :path (path-to dest dij)}))
