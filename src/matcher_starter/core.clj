(ns matcher-starter.core
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(def numbers '(3 4 1 2 8 6
               6 1 8 2 7 4
               5 9 3 9 9 5
               8 4 1 3 2 6
               3 7 2 8 6 4
               ))

(defn make-matrix [row col numbers]
  (cond
    (= 0 row)
      nil
    :else
    (cons (take col numbers)
         (make-matrix (- row 1) col (drop col numbers)))))


(def matrix (make-matrix 5 6 numbers))

(defn get-at [matrix x y]
  (nth (nth matrix y) x))

(defn move-up-right [matrix x y]
(if (= y 0)
  (get-at (+ x 1) (- (count matrix) 1) matrix)
  (get-at (+ x 1)  (- y 1) matrix) )
  )

(defn move-right [matrix x y]
  (get-at (+ x 1) y matrix))

(defn move-down-right [matrix x y]
  (if (= y (- (count matrix) 1))
    (get-at (+ x 1) 0 matrix)
    (get-at (+ x 1) (+ y 1) matrix))
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
      (let [value (compare-three (sum-from-pos matrix x (- y 1))
                                 (sum-from-pos matrix x y)
                                 (sum-from-pos matrix x (+ y 1)))]
        (list (get-at matrix x y) (case value
                                    0 (traverse matrix (+ x 1) (- y 1))
                                    1 (traverse matrix (+ x 1) y)
                                    2 (traverse matrix (+ x 1) (+ y 1)))))
    )))

(defn output [mess]
  {
   :path
   (flatten mess)
   :total
   (reduce + (flatten mess))
   })
