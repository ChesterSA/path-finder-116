(ns matcher-starter.tail-recursion
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))
;sample input
;(dijkstra-search 5 6  '(3 4 1 2 8 6
;                        6 1 8 2 7 4
;                        5 9 3 9 9 5
;                        8 4 1 3 2 6
;                        3 7 2 8 6 4))
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

(defn get-top-path [matrix x y]
  (if (= y 0)
    (get-at matrix (+ x 1) (- (count matrix) 1))
    (get-at matrix (+ x 1) (- y 1)) )
  )

(defn get-middle-path [matrix x y]
  (get-at matrix (+ x 1) y))

(defn get-bottom-path [matrix x y]
  (if (= y (- (count matrix) 1))
    (get-at matrix (+ x 1) 0)
    (get-at matrix (+ x 1) (+ y 1)))
  )

(defn get-possible-paths [current-x, current-y, matrix]
    ;Helper function that uses the path variable functions
    ;to build a list of possible routes.
    [(get-top-path matrix current-x current-y)
     (get-middle-path matrix current-x current-y)
     (get-bottom-path matrix current-x current-y)])

(defn compare-three [lis]
  (let [val (apply min lis)]
    (cond
      (= val (first lis))
      -1
      (= val (second lis))
      0
      :else
      1)
    ))

(defn wrap [y matrix]
  (cond
    (< y 0) (+ (count (matrix)) 1)
    (= (count matrix) y) 0)
    :else y)

(defn traverse
  ([rows, columns, matrix]
   (let [current-y (first (apply min-key first (make-matrix rows columns matrix)))                                                 ;Assigns current value and current position using
         current-x 0]
     (traverse matrix current-x 0 current-y (cons  current-y '() ))))

  ([matrix, current-x, current-y, total, path]                                        ;Iterates through each column, and decreases the
   (if (= (+ 1 current-y) (count matrix))                                                                                     ;row count each time.
     {:total total :path (reverse path)}

     (let [values (get-possible-paths current-x,current-y, matrix)
           next-value (apply min values)
           move (compare-three values)
           next-y (wrap (+ current-y move) matrix)]

       (recur matrix (+ current-x 1) next-y  (+ total next-value) (cons next-value path)))
     ))
  )