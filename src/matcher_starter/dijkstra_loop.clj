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
    {:path (improve-path matrix (path-to dest dijk))
     :total (cost-to dest dijk)}))

(defn get-start-end [row col matrix]
  "gets the list of start and end nodes for the matrix given"
  (for [i (range 0 row)]
    [(for [j (range 0 row)]
                    (path-finder-old matrix (get-id 0 i) (get-id (dec col) j)))]))

(defn path-finder [row col numbers]
  "returns the total and path for the matrix given"
  (apply min-key :total (flatten (get-start-end row col (make-matrix2 col numbers)))))

(defn tester [tests]
  (mfor ['(?id ?test => ?res) tests]
        (println (? id))
        (time (if-not (= (eval (? test)) (eval (? res)))
                (println (mout
                           '(FAILED ?id => ?res))))))
  'end-of-testing)

(def matrix-tests
  '( [matrix-20 (path-finder 5 4 '(7 2 7 10 7 5 7 10 5 8 4 2 1 4 3 3 9 8 5 9)) => '{:path (9 4 3 2), :total 9}]
     [matrix-25 (path-finder 5 5 '(6 1 2 10 3 9 4 9 8 8 7 4 1 3 6 7 2 1 10 5 10 9 2 8 2)) => '{:path (10 2 1 3 5), :total 11}]
     [matrix-100 (path-finder 10 10 '(4 9 6 3 7 1 2 6 10 2 7 2 7
                                       7 3 4 9 6 4 3 1 10 1 1 5 7 4 3 3 6 5 8 5 6 4 1 3 1 10 7
                                       8 8 8 6 2 4 7 2 2 7 5 7 9 1 9 6 4 10 1 1 3 3 1 8 7 9 3
                                       2 10 5 9 6 3 2 9 10 6 10 4 6 5 2 7 6 6 3 6 3 9 1 3 8 8
                                       4 10 6 9 2 4 10)) => '{:path (9 3 1 1 2 1 3 2 1 1), :total 15}]
     [matrix-30-1 (path-finder 2 15 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2)) => '{:path (7 2 4 4 1 3 2 1 3 2 2 1 6 2 1), :total 34}]
     [matrix-30-2 (path-finder 5 6 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2)) => '{:path (10 1 1 3 2 2), :total 9}]
     [matrix-50 (path-finder 10 5 '(4 3 5 8 2 9 4 5 5 3 6 7 3 8 4 8 6 8 7 10 1 5 4 3 10 10 1 8 1 3 9
                                     3 6 10 8 8 4 4 9 2 10 9 4 3 9 6 8 10 10 6)) => ]
     [matrix-30-3 (path-finder 15 2 '(5 7 3 9 8 2 8 10 10 2 10 3 8 8 6 2 7 2 10 4 1 10 10 1 5 6 3 9 4 3)) => ]
     [matrix-45 (path-finder 5 9 '(8 6 3 7 1 7 6 1 6 9 9 7 5 8 1 4 9 7 8 1 6 5 4 1 2 4 2 9 4 1 9 3 2 3 1 10 1 3 5 1 2 5 7 10 3)) => '{:path (9 1 1 1 1 1 2 1 2), :total 10}]
     ;[empty-matrix (path-finder 0 0 '()) => 9]
     [matrix-1000 (path-finder 10 100 '(10 9 8 3 5 3 9 10 8 1 10 9 1 6 3 3 1 5 5 6 5 9 6 9 2 6 8 10 1 9 10 6 1 8 8 9 4 1 3 5 1 3 2 9 2 2 4 10 3 10 7 8 3 9 8 9 10 6 5 8 5 2 9 8 8 4
                                         10 7 10 8 10 6 9 5 2 9 8 9 8 10 5 3 1 9 5 2 3 6 8 7 6 7 4 10 4 3 5 6 6 4 7 3 8 7 5 6 10 2 1 6 3 8 9 8 1 5 4 2 3 6 3 7 6 1 1 1 7 4 7 2 10 1 3
                                         1 10 8 6 10 1 6 5 9 4 7 9 8 4 8 3 6 9 3 1 6 1 10 2 5 5 5 2 9 3 1 8 1 8 9 8 8 4 1 7 8 6 6 3 5 5 4 4 4 8 1 5 1 2 7 6 8 10 1 9 1 7 1 5 8 10 10 3
                                         3 4 7 5 5 3 1 10 4 7 8 10 5 7 3 5 9 8 7 5 4 1 2 5 6 6 7 3 7 1 5 2 8 9 3 7 6 7 4 9 6 9 9 1 1 10 2 3 6 7 2 7 5 6 3 6 10 5 1 9 6 4 7 2 1 8 4 1 5
                                         8 8 6 4 4 9 7 7 4 6 6 4 7 3 7 3 1 8 7 1 2 1 3 8 1 3 2 5 5 5 6 5 1 9 3 6 9 4 10 8 8 5 10 1 6 3 7 6 4 6 4 6 8 2 7 4 8 5 6 3 1 10 8 10 10 5 8 3 2
                                         2 1 9 2 2 10 9 5 5 4 6 5 2 7 10 9 10 6 10 4 5 5 6 9 10 5 3 3 5 3 9 3 10 5 8 7 5 6 6 1 7 1 7 1 5 8 8 10 7 3 10 1 2 7 1 3 2 2 10 10 5 1 10 7 5 5
                                         6 9 4 6 4 1 3 1 4 7 7 7 4 9 7 4 10 4 1 7 1 7 7 2 1 8 7 7 1 3 2 10 8 2 8 4 8 4 5 8 8 9 8 3 3 7 10 7 2 7 1 7 5 1 1 6 6 3 10 7 5 8 6 6 8 3 10 5 2
                                         2 7 4 6 7 5 6 9 6 2 4 3 6 7 3 1 1 9 1 2 10 1 4 6 10 9 10 10 8 6 4 9 5 8 5 5 9 8 7 7 10 6 3 8 6 5 5 7 9 4 7 8 9 7 3 10 8 4 3 6 9 9 6 6 4 7 1 5 2
                                         7 5 8 3 1 8 8 1 1 4 3 2 2 3 8 4 6 5 3 6 3 6 3 8 1 1 2 2 6 1 5 4 9 1 2 6 1 9 3 9 9 10 10 9 9 2 2 3 3 5 9 9 3 6 5 4 2 9 4 4 2 5 10 10 2 4 4 1 2 5
                                         2 4 10 7 9 8 8 9 9 9 2 1 10 7 5 9 3 1 1 1 3 7 3 7 10 5 7 2 3 3 2 7 7 4 2 10 4 8 7 2 10 4 3 1 5 7 1 6 2 3 7 8 3 6 8 3 1 3 8 5 7 6 5 7 7 7 3 4 6 2
                                         8 1 3 2 9 5 7 8 1 10 2 4 4 1 8 9 6 10 9 7 5 4 8 10 2 8 9 9 6 1 8 4 9 7 6 6 3 10 5 10 10 7 4 7 6 5 5 9 9 7 8 7 4 7 5 4 3 6 2 7 1 9 1 10 1 6 3 5 10
                                         10 10 6 9 4 7 3 10 10 9 7 9 1 2 5 5 8 6 7 8 3 8 1 10 3 2 4 1 4 8 8 1 10 7 5 1 9 4 6 9 3 6 1 9 2 5 4 10 8 2 5 10 5 5 6 7 7 3 10 6 9 7 10 5 4 3 10 5
                                         1 5 4 4 4 9 5 6 10 5 6 7 9 2 8 10 10 2 10 2 2 4 6 7 4 6 9 7 8 1 3 6 10 10 6 7 10 9 7 2 1 3 4 7 7 6 8 5 4 9 5 2 7 9 4 6 6 7 10 7 4 6 8 1 6 5 9 4 6 8
                                         4 4 2 2 10 10 5 1 7 3 8 7 3 6 8 1 5 3 2 10 10 6 4 4 8 6 10 3 10 1 6 6 2 6 8 3 7 8 2 7 1 10 10 8 4 5 2 1 6 7 6 5 3 1 5 2 2 2 10 5 7 3 4 2 6 8 5 8 4
                                         3 9 2 1 9 3 3 5 1 7 1 2 5 10 3 2 6 9 5 2 7 7 4 6 2 4 8 1 6 8 7 10 6 3 10 4 9 10 5 10 7))
      => '{:path (6 3 1 5 3 5 3 1 1 1 3 8 1 4 3 3 1 2 3 6 3 4 1 1 1 1 6 4 1 2 1 1 1 1 8 5 2 1 1 5 1 3 2 1 4 1 4 1 3 3 1 1 2 3 2 1 3 5 4 1 2 2 3 1 2 1 3 6 1 1 2 6 1 2 4 5 3 2 4 1 4 3 2 1 1 3
                   2 1 5 2 1 2 1 1 1 2 2 6 4 2), :total 251}]
     [matrix-50-BIG (path-finder 10 5 '(59 64 77 68 24 94 88 78 66 99 75 34 98 97 69 73 55 42 74 42 4 97 100 56 43 34 56 33 12 2 88 50 80 6 70 92 12 63 67 92 30 81 26 36 23 44 2 10 88 91))
      => ]
     [matrix-ones (path-finder 10 3 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) =>  ]
     ))
