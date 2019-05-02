(ns matcher-starter.look-ahead-params
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
  (cond
    (= 0 row)
    nil
    :else
    (cons (take col numbers)
          (make-matrix (- row 1) col (drop col numbers)))))
  
(def matrix (make-matrix 5 6 numbers))
(def matrix-wrap (make-matrix 5 6 wrap))

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

(defn look-ahead [matrix x y num]
  "Returns the value of the best path for the next num moves from cell x,y"
  (if (or (= num 0) (= x (dec (count (first matrix)))))
    (get-at matrix x y)
    (let [up (move-up-right matrix x y)
          right (move-right x y)
          down (move-down-right matrix x y)]
      (+ (get-at matrix x y)
         (apply min(list (look-ahead matrix (getx up) (gety up) (dec num))
                  (look-ahead matrix (getx right) (gety right) (dec num))
                  (look-ahead matrix (getx down) (gety down) (dec num))))))))

(defn traverse
  "moves through the matrix according to our algorithm"
  ([matrix x y num]
   (cond
     (= x (dec (count (first matrix))))
     (get-at matrix x y)
     :else
     (let [up (move-up-right matrix x y)
           right (move-right x y)
           down (move-down-right matrix x y)
           value (compare-three (look-ahead matrix (getx up) (gety up) num)
                                (look-ahead matrix (getx right) (gety right) num)
                                (look-ahead matrix (getx down) (gety down)num))]
       (list (get-at matrix x y) (case value
                                   0 (traverse matrix (inc x) (gety up) num)
                                   1 (traverse matrix (inc x) (gety right) num)
                                   2 (traverse matrix (inc x) (gety down) num)))))
     ))

(defn output [raw]
  "Formats the raw output into something readable"
  {
   :path
   (flatten raw)
   :total
   (reduce + (flatten raw))
   }
  )

(defn all-paths [matrix num]
  "Generates the shortest path from each starting position in the matrix"
  (map
    #(output(traverse matrix 0 % num))
    (range (count  matrix))
    ))

(defn min-weight-path [matrix num]
  "Gets the path with lowest total from list of all paths"
  (apply min-key :total (all-paths matrix num)))

(defn path-finder [row col numbers num]
  "generates a matrix of dimensions row*col,
  then returns the shortest path and cost of that path"
  (min-weight-path (make-matrix row col numbers) num))

(defn tester [tests]
  (mfor ['(?id ?test => ?res) tests]
        (println (? id))
        (time (if-not (= (eval (? test)) (eval (? res)))
                (println (mout
                           '(FAILED ?id => ?res))))))
  'end-of-testing)

(def matrix-tests
  '( [matrix-20-3 (path-finder 5 4 '(7 2 7 10 7 5 7 10 5 8 4 2 1 4 3 3 9 8 5 9) 3) => '{:path (1 4 3 2), :total 10}]
     [matrix-25-3 (path-finder 5 5 '(6 1 2 10 3 9 4 9 8 8 7 4 1 3 6 7 2 1 10 5 10 9 2 8 2) 3) => '{:path (7 2 1 3 5), :total 18}]
     [matrix-100-3 (path-finder 10 10 '(4 9 6 3 7 1 2 6 10 2 7 2 7
                                       7 3 4 9 6 4 3 1 10 1 1 5 7 4 3 3 6 5 8 5 6 4 1 3 1 10 7
                                       8 8 8 6 2 4 7 2 2 7 5 7 9 1 9 6 4 10 1 1 3 3 1 8 7 9 3
                                       2 10 5 9 6 3 2 9 10 6 10 4 6 5 2 7 6 6 3 6 3 9 1 3 8 8
                                       4 10 6 9 2 4 10) 3) => '{:path (3 3 1 1 2 1 3 1 2 1), :total 18}]
     [matrix-30-1-3 (path-finder 2 15 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2) 3) => '{:path (7 2 4 4 1 3 2 1 3 2 2 1 6 2 1), :total 41}]
     [matrix-30-2-3 (path-finder 5 6 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2) 3) => '{:path (2 1 1 5 1 1), :total 11}]
     [matrix-50-3 (path-finder 10 5 '(4 3 5 8 2 9 4 5 5 3 6 7 3 8 4 8 6 8 7 10 1 5 4 3 10 10 1 8 1 3 9
                                     3 6 10 8 8 4 4 9 2 10 9 4 3 9 6 8 10 10 6) 3) => '{:path (1 1 4 1 3), :total 10}]
     [matrix-30-3-3 (path-finder 15 2 '(5 7 3 9 8 2 8 10 10 2 10 3 8 8 6 2 7 2 10 4 1 10 10 1 5 6 3 9 4 3) 3) => '{:path (1 1), :total 2}]
     [matrix-45-3 (path-finder 5 9 '(8 6 3 7 1 7 6 1 6 9 9 7 5 8 1 4 9 7 8 1 6 5 4 1 2 4 2 9 4 1 9 3 2 3 1 10 1 3 5 1 2 5 7 10 3) 3) => '{:path (1 3 1 1 1 1 2 1 2), :total 13}]
     ;[empty-matrix (path-finder 0 0 '()) => 9]
     [matrix-1000-3 (path-finder 10 100 '(10 9 8 3 5 3 9 10 8 1 10 9 1 6 3 3 1 5 5 6 5 9 6 9 2 6 8 10 1 9 10 6 1 8 8 9 4 1 3 5 1 3 2 9 2 2 4 10 3 10 7 8 3 9 8 9 10 6 5 8 5 2 9 8 8 4
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
                                         3 9 2 1 9 3 3 5 1 7 1 2 5 10 3 2 6 9 5 2 7 7 4 6 2 4 8 1 6 8 7 10 6 3 10 4 9 10 5 10 7) 3)
      => '{:path (1 5 1 5 3 5 3 1 1 1 3 8 1 4 3 3 1 2 5 1 1 6 6 1 1 1 6 4 1 2 1 1 1 1 8 5 2 1 1 5 1 3 2 1 2 2 2 2 3 5 3 3 3 6 1 3 2 6 4 2 1 2 1 7 1 2 6 1 1 1 2 6 1 2 2 7 1 6 1 5 1 4 1 3 5 1 1
                   7 3 1 1 2 1 1 1 1 2 5 5 5), :total 275}]
     [matrix-50-BIG-3 (path-finder 10 5 '(59 64 77 68 24 94 88 78 66 99 75 34 98 97 69 73 55 42 74 42 4 97 100 56 43 34 56 33 12 2 88 50 80 6 70 92 12 63 67 92 30 81 26 36 23 44 2 10 88 91) 3)
      => '{:path (30 2 10 36 23), :total 101}]
     [matrix-ones-3 (path-finder 10 3 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 3) =>  '{:path (1 1 1), :total 3}]

     [matrix-20-4 (path-finder 5 4 '(7 2 7 10 7 5 7 10 5 8 4 2 1 4 3 3 9 8 5 9) 4) => '{:path (1 4 3 2), :total 10}]
     [matrix-25-4 (path-finder 5 5 '(6 1 2 10 3 9 4 9 8 8 7 4 1 3 6 7 2 1 10 5 10 9 2 8 2) 4) => '{:path (7 2 1 3 5), :total 18}]
     [matrix-100-4 (path-finder 10 10 '(4 9 6 3 7 1 2 6 10 2 7 2 7
                                         7 3 4 9 6 4 3 1 10 1 1 5 7 4 3 3 6 5 8 5 6 4 1 3 1 10 7
                                         8 8 8 6 2 4 7 2 2 7 5 7 9 1 9 6 4 10 1 1 3 3 1 8 7 9 3
                                         2 10 5 9 6 3 2 9 10 6 10 4 6 5 2 7 6 6 3 6 3 9 1 3 8 8
                                         4 10 6 9 2 4 10) 4) => '{:path (1 2 1 1 4 1 3 1 2 1), :total 17}]
     [matrix-30-1-4 (path-finder 2 15 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2) 4) => '{:path (7 2 4 4 1 3 2 1 3 2 2 1 6 2 1), :total 41}]
     [matrix-30-2-4 (path-finder 5 6 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2) 4) => '{:path (2 1 1 5 1 1), :total 11}]
     [matrix-50-4 (path-finder 10 5 '(4 3 5 8 2 9 4 5 5 3 6 7 3 8 4 8 6 8 7 10 1 5 4 3 10 10 1 8 1 3 9
                                       3 6 10 8 8 4 4 9 2 10 9 4 3 9 6 8 10 10 6) 4) => '{:path (1 1 4 1 3), :total 10}]
     [matrix-30-3-4 (path-finder 15 2 '(5 7 3 9 8 2 8 10 10 2 10 3 8 8 6 2 7 2 10 4 1 10 10 1 5 6 3 9 4 3) 4) => '{:path (1 1), :total 2}]
     [matrix-45-4 (path-finder 5 9 '(8 6 3 7 1 7 6 1 6 9 9 7 5 8 1 4 9 7 8 1 6 5 4 1 2 4 2 9 4 1 9 3 2 3 1 10 1 3 5 1 2 5 7 10 3) 4) => '{:path (1 3 1 1 1 1 2 1 2), :total 13}]
     [matrix-1000-4 (path-finder 10 100 '(10 9 8 3 5 3 9 10 8 1 10 9 1 6 3 3 1 5 5 6 5 9 6 9 2 6 8 10 1 9 10 6 1 8 8 9 4 1 3 5 1 3 2 9 2 2 4 10 3 10 7 8 3 9 8 9 10 6 5 8 5 2 9 8 8 4
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
                                           3 9 2 1 9 3 3 5 1 7 1 2 5 10 3 2 6 9 5 2 7 7 4 6 2 4 8 1 6 8 7 10 6 3 10 4 9 10 5 10 7) 4)
      => '{:path (1 5 1 5 3 5 3 1 1 1 3 8 1 4 3 3 1 2 5 1 1 6 6 1 1 1 6 4 1 2 1 1 1 1 8 5 2 1 1 5 1 3 2 1 2 2 2 2 3 5 3 3 3 6 1 3 2 6 4 2 1 2 1 7 1 2 6 1 1 1 2 6 1 2 2 7 1 2 6 1 4 3 2 1 1 3 2
                   1 5 2 1 2 1 1 1 2 2 6 4 2),   :total 267}]
     [matrix-50-BIG-4 (path-finder 10 5 '(59 64 77 68 24 94 88 78 66 99 75 34 98 97 69 73 55 42 74 42 4 97 100 56 43 34 56 33 12 2 88 50 80 6 70 92 12 63 67 92 30 81 26 36 23 44 2 10 88 91) 4)
      => '{:path (30 2 10 36 23), :total 101}]
     [matrix-ones-4 (path-finder 10 3 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 4) =>  '{:path (1 1 1), :total 3}]
     ))