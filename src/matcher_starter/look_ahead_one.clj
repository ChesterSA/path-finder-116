(ns matcher-starter.look-ahead-one
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(use 'org.clojars.cognesence.matcher.core)

(def numbers '(3 4 1 2 8 6
                6 1 8 2 7 4
                5 9 3 9 9 5
                8 4 1 3 2 6
                3 7 2 8 6 4
                ))

(def wrap '(3 4 1 2 8 6
             6 1 8 2 7 4
             5 9 3 9 9 5
             8 4 1 3 2 6
             3 7 2 1 2 3
             ))

(defn make-matrix [row col numbers]
  "forms a nested list with row*col dimensions"
  (cond
    (= 0 row)
    nil
    :else
    (cons (take col numbers)
          (make-matrix (- row 1) col (drop col numbers)))))

(defn getx [coord]
  "Gets the x coordinate from a (x y) coord list"
  (first coord))

(defn gety [coord]
  "Gets the y coordinate from a (x y) coord list"
  (first (rest coord)))

(defn get-at [matrix x y]
  "Gets the value at position x,y in the matrix"
  (nth (nth matrix y) x))

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

(defn compare-three [a b c]
  "Compares three values and returns 0,1,or 2 depending which is bigger"
  (let [val (min a b c)]
    (cond
      (= val a)
      0
      (= val b)
      1
      (= val c)
      2)
    ))

(defn traverse
  "moves through the matrix according to our algorithm"
  ([matrix x y]
   (cond
     (= x (dec (count (first matrix))))
     (get-at matrix x y)
     :else
     (let [up (move-up-right matrix x y)
           right (move-right x y)
           down (move-down-right matrix x y)
           value (compare-three (get-at matrix (getx up) (gety up))
                                (get-at matrix (getx right) (gety right))
                                (get-at matrix (getx down) (gety down)))]
       (list (get-at matrix x y) (case value
                                   0 (traverse matrix (inc x) (gety up))
                                   1 (traverse matrix (inc x) (gety right))
                                   2 (traverse matrix (inc x) (gety down))))))
    ))

(defn output [raw]
  "Formats the raw output into something readable"
  (println raw)
  {
   :path
   (flatten raw)
   :total
   (reduce + (flatten raw))
   }
  )

(defn all-paths [matrix]
  "Generates the shortest path from each starting position in the matrix"
  (map
    #(output(traverse matrix 0 %))
    (range (count  matrix))
    ))

(defn min-weight-path [matrix]
  "Gets the path with lowest total from list of all paths"
  (apply min-key :total (all-paths matrix)))

(defn path-finder [row col numbers]
    "generates a matrix of dimensions row*col,
    then returns the shortest path and cost of that path"
  (min-weight-path (make-matrix row col numbers)))