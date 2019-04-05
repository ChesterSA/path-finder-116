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


(def one-row (make-matrix 1 6 '(3 4 1 6 7 4)))

(def two-row (make-matrix 3 7 '(3 5 4 6 3 5 7
                                 4 3 5 7 8 5 4
                                 4 3 6 7 5 7 8)))

(def matrix (make-matrix 6 5 numbers))
(def matrix-wrap (make-matrix 5 6 wrap))

(defn getx [coord]
  (first coord))

(defn gety [coord]
  (first (rest coord)))

(defn get-at [matrix x y]
  (nth (nth matrix y) x))

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

(defn compare-three [a b c]
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

(defn output [mess]
  (println mess)
  {
   :path
   (flatten mess)
   :total
   (reduce + (flatten mess))
   }
  )

(defn all-paths [matrix]
  (map
    #(output(traverse matrix 0 %))
    (range (count  matrix))
    ))

(defn min-weight-path [matrix]
  (apply min-key :total (all-paths matrix)))

(defn path-finder [row col numbers]
  (min-weight-path (make-matrix row col numbers)))