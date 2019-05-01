(ns matcher-starter.dijkstra-loop
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(require '[matcher-starter.priority_map :refer [priority-map priority-map-keyfn]])
(require '[clojure.string :as str])

; '''Modification of Djikstra''' [[Media:https://www.ummels.de/2014/06/08/dijkstra-in-clojure/]]
(use 'org.clojars.cognesence.matcher.core)

(def numbers '(3 4 1 2 8 6
                6 1 8 2 7 4
                5 9 3 9 9 5
                8 4 1 3 2 6
                3 7 2 8 6 4
                ))

(defn make-matrix [row col numbers]
  "forms a nested list with row*col dimensions"
  (cond
    (= 0 row)
    nil
    :else
    (cons (take col numbers)
          (make-matrix (- row 1) col (drop col numbers)))))

(defn make-matrix2 [col numbers]
  "forms a nested list with row*col dimensions, uses clojure partition"
  (partition col numbers))

(def matrix (make-matrix 5 6 numbers))

(defn get-at
  "Returns the number in the matrix at a specified position
  Coordinates can be in 5 6, or \"5-6\" format"
  ([matrix x y]
  (nth (nth matrix y) x))
  ([matrix coord]
  (let [coords (str/split coord #"-")]
    (get-at matrix (Integer/parseInt (first coords)) (Integer/parseInt (first (rest coords)))))))

(defn get-id [x y]
  "Turns x y into \"x-y\" format"
  (str x "-" y))

(defn make-edge [matrix start finish]
  "Given two coord values, returns a map representing their edge"
  (let [start-x (first start)
        start-y (second start)
        finish-x (first finish)
        finish-y (second finish)]
    [(get-id start-x start-y) (get-id finish-x finish-y) (get-at matrix finish-x finish-y)]))

(defn move-up-right [matrix x y]
  "returns the coordinate when move up-right from x,y.
  Wraps if necessary"
  (if (= y 0)
    (list (+ x 1) (- (count matrix) 1))
    (list  (+ x 1)  (- y 1)) )
  )

(defn move-right [x y]
  "returns the coordinate when move right from x,y."
  (list (+ x 1) y))

(defn move-down-right [matrix x y]
  "returns the coordinate when move down-right from x,y.
  Wraps if necessary"
  (if (= y (- (count matrix) 1))
    (list (+ x 1) 0)
    (list(+ x 1) (+ y 1)))
  )

(defn make-three-edges [matrix x y]
  "Generates the edges for all three potential moves from cell x,y"
  (list (make-edge matrix (list x y) (move-up-right matrix x y))
   (make-edge matrix (list x y) (move-right x y))
   (make-edge matrix (list x y) (move-down-right matrix x y))))

(defn make-node-list
  "Makes a list of all the nodes in the matrix given"
  [x y finalx finaly]
    (cond
      (and (= x finalx) (= y finaly))
        (get-id x y)
      (= x finalx)
        (list (get-id x y) (make-node-list 0 (inc y) finalx finaly))
      :else
        (list (get-id x y) (make-node-list (inc x) y finalx finaly))))

(defn make-nodes [matrix]
  "formats the given list of nodes into a flattened map"
  (into [] (flatten (make-node-list 0 0 (dec (count (first matrix))) (dec (count matrix))))))

(defn make-edge-list
  "generates a badly formatted list of all edges in the matrix"
  [matrix x y finalx finaly]
  (cond
    (and (= x finalx) (= y finaly))
      (make-three-edges matrix x y)
    (= x finalx)
      (list (make-edge matrix (list x y) (move-up-right matrix x y)) (make-edge matrix (list x y) (move-right x y)) (make-edge matrix (list x y) (move-down-right matrix x y)) (make-edge-list matrix 0 (inc y) finalx finaly))
    :else
      (list (make-edge matrix (list x y) (move-up-right matrix x y)) (make-edge matrix (list x y) (move-right x y)) (make-edge matrix (list x y) (move-down-right matrix x y)) (make-edge-list matrix (inc x) y finalx finaly))))

(defn make-edges [matrix]
  "formats the list of edges into groups of three needed"
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
  [nodes edges]
  (let [init-graph (reduce merge (map #(hash-map %1 {}) nodes))]
    (reduce #(merge-with merge %1 %2)
            init-graph
            (map #(hash-map (nth % 0) (hash-map (nth % 1) (nth % 2))) edges))))

(defn path-to [goal dijk]
  "Returns the given path from start to goal"
  (if (contains? dijk goal)
    (reverse (take-while identity (iterate (comp second dijk) goal)))
    nil))

(defn improve-path [matrix path]
  "formats the path into the output the spec wants"
  (map #(get-at matrix %) path))

(defn cost-to [goal dijk]
  "Returns the cost of the best pth from start to goal"
  (if (contains? dijk goal)
    (first (dijk goal))
    -1))

(defn path-finder-old [matrix start dest]
  "Returns the list of all potential paths in the matrix, from start col to end col"
  (let [graph (make-adj-list (make-nodes matrix) (make-edges matrix))
        dijk (dijkstra start graph)]
    {:total (cost-to dest dijk)
     :path (improve-path matrix (path-to dest dijk))}))

(defn get-start-end [row col matrix]
  "gets the list of start and end nodes for the matrix given"
  (for [i (range 0 row)]
    [(for [j (range 0 row)]
                    (path-finder-old matrix (get-id 0 i) (get-id (dec col) j)))]))

(defn path-finder [row col numbers]
  "returns the total and path for the matrix given"
  (apply min-key :total (flatten (get-start-end row col (make-matrix2 col numbers)))))
