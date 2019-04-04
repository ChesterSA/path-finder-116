(ns matcher-starter.TANC
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

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

(defn move-right [matrix x y]
  (list (+ x 1) y))

(defn move-down-right [matrix x y]
  (if (= y (- (count matrix) 1))
    (list (+ x 1) 0)
    (list(+ x 1) (+ y 1)))
  )

(defn sum-from-pos [matrix x y]
  (cond
    (= x (count matrix))
    (get-at matrix x y)
    :else
    (reduce + (list (get-at matrix x y) (sum-from-pos matrix (+ x 1) y))))
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
  ([matrix]
    (traverse matrix 0 0))
  ([matrix x y]
    (cond
      (= x (count matrix))
        (get-at matrix x y)
      :else
      (let [up (move-up-right matrix x y)
            right (move-right matrix x y)
            down (move-down-right matrix x y)
            value (compare-three (sum-from-pos matrix (getx up) (gety up))
                                 (sum-from-pos matrix (getx right) (gety right))
                                 (sum-from-pos matrix (getx down) (gety down)))]
        (list (get-at matrix x y) (case value
                                    0 (traverse matrix (+ x 1) (gety up))
                                    1 (traverse matrix (+ x 1) (gety right))
                                    2 (traverse matrix (+ x 1) (gety down)))))
    )))

(defn output [mess]
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
       (range (- (count (first matrix)) 1))
       ))

(defn min-weight-path [matrix]
  (apply min-key :total (all-paths matrix)))

(defn path-finder [row col numbers]
  (min-weight-path (make-matrix row col numbers)))