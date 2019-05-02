(ns matcher-starter.tail-recursion
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(def numbers '(3 4 1 2 8 6
                6 1 8 2 7 4
                5 9 3 9 9 5
                8 4 1 3 2 6
                3 7 2 8 6 4
                ))

(def tom '(3 4 1 2 8 6 6
           1 8 2 7 4 5 9
           3 9 9 5 8 4 1
           3 2 6 3 7 2 8
           6 4 1 2 3 4 5
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
  ;(println "get-at" x y)
  (nth (nth matrix y) x))

(defn get-top-path [matrix x y]
  ;(println "get-top" x y)
  (if (<= y 0)
    (get-at matrix (+ x 1) (- (count matrix) 1))
    (get-at matrix (+ x 1) (- y 1)) )
  )

(defn get-middle-path [matrix x y]
  ;(println "get-middle" x y)
  (get-at matrix (+ x 1) y))

(defn get-bottom-path [matrix x y]
  ;(println "get-bottom" x y)
  (if (>= y (- (count matrix) 1))
    (get-at matrix (+ x 1) 0)
    (get-at matrix (+ x 1) (+ y 1)))
  )

(defn get-possible-paths [current-x, current-y, matrix]
  ;(println "get-poss-paths" current-x current-y matrix)
  ;Helper function that uses the path variable functions
  ;to build a list of possible routes.
  [(get-top-path matrix current-x current-y)
   (get-middle-path matrix current-x current-y)
   (get-bottom-path matrix current-x current-y)])

(defn compare-three [lis]
  ;(println "compare-three" lis)
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
  ;(println "wrap" y matrix)
  (cond
    (<= y 0) (- (count matrix) 1)
    (= (count matrix) y)  0
  :else y))

(defn traverse
  [matrix, current-x, current-y, total, path]  ;Iterates through each column, and decreases the
  ;(println "traverse" current-x current-y path)
  (if (= (+ 1 current-x) (count (first matrix)))                                                                                     ;row count each time.
    {:path (reverse path) :total total}

    (let [values (get-possible-paths current-x,current-y, matrix)
          next-value (apply min values)
          move (compare-three values)
          next-y (wrap (+ current-y move) matrix)]
      ;(println "next-y:" next-y  "move:" move)
      (recur matrix (+ current-x 1) next-y  (+ total next-value) (cons next-value path)))
    ))

(defn get-start [y i lis]
  ;(println "get-start" y i lis)
  (if (= y (nth lis i))
    i
    (get-start y (inc i) lis)))

(defn path-finder [row col numbers]
  (let [matrix (make-matrix row col numbers)
        current-x 0
        total (first (apply min-key first matrix))
        current-y  (get-start total 0 (map first matrix))
        ]                                             ;Assigns current value and current position usin
    (traverse matrix current-x current-y 0 (cons current-y '() ))))

(defn tester [tests]
  (mfor ['(?id ?test => ?res) tests]
        (println (? id))
        (time (if-not (= (eval (? test)) (eval (? res)))
                (println (mout
                           '(FAILED ?id => ?res))))))
  'end-of-testing)

(def matrix-tests
  '( [matrix-20 (path-finder 5 4 '(7 2 7 10 7 5 7 10 5 8 4 2 1 4 3 3 9 8 5 9)) => '{:path (3 4 3 2), :total 9}]
     [matrix-25 (path-finder 5 5 '(6 1 2 10 3 9 4 9 8 8 7 4 1 3 6 7 2 1 10 5 10 9 2 8 2)) => '{:path (0 1 1 3 5), :total 10}]
     [matrix-100 (path-finder 10 10 '(4 9 6 3 7 1 2 6 10 2 7 2 7
                                       7 3 4 9 6 4 3 1 10 1 1 5 7 4 3 3 6 5 8 5 6 4 1 3 1 10 7
                                       8 8 8 6 2 4 7 2 2 7 5 7 9 1 9 6 4 10 1 1 3 3 1 8 7 9 3
                                       2 10 5 9 6 3 2 9 10 6 10 4 6 5 2 7 6 6 3 6 3 9 1 3 8 8
                                       4 10 6 9 2 4 10)) => '{:path (2 2 1 1 3 1 2 2 4 1), :total 17}]
     [matrix-30-1 (path-finder 2 15 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2)) => '{:path (1 2 4 4 1 3 2 1 3 2 2 1 6 2 1), :total 34}]
     [matrix-30-2 (path-finder 5 6 '(9 2 4 10 1 3 2 1 3 5 7 1 10 2 1 7 5 4 4 9 6 3 9 10 2 2 4 6 2 2)) => '{:path (1 1 1 3 2 2), :total 9}]
     [matrix-50 (path-finder 10 5 '(4 3 5 8 2 9 4 5 5 3 6 7 3 8 4 8 6 8 7 10 1 5 4 3 10 10 1 8 1 3 9
                                     3 6 10 8 8 4 4 9 2 10 9 4 3 9 6 8 10 10 6)) => '{:path (4 1 4 1 3), :total 9}]
     [matrix-30-3 (path-finder 15 2 '(5 7 3 9 8 2 8 10 10 2 10 3 8 8 6 2 7 2 10 4 1 10 10 1 5 6 3 9 4 3)) => '{:path (10 1), :total 1}]
     [matrix-45 (path-finder 5 9 '(8 6 3 7 1 7 6 1 6 9 9 7 5 8 1 4 9 7 8 1 6 5 4 1 2 4 2 9 4 1 9 3 2 3 1 10 1 3 5 1 2 5 7 10 3)) => '{:path (4 3 1 1 1 1 2 1 2), :total 12}]
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
      => '{:path (4 5 1 5 3 5 3 1 1 1 10 6 1 2 4 6 3 4 4 6 4 4 1 1 1 1 6 4 1 5 1 6 1 2 4 5 2 1 3 5 1 3 2 1 2 2 2 2 3 5 3 3 3 2 6 7 4 5 3 1 5 2 3 3 6 3 6 1 1 1 2 6 1 2 2 5 4 2 6 1 4 3 2 1 1
                   3 2 1 5 2 1 2 1 1 1 1 2 5 5 5), :total 299}]
     [matrix-50-BIG (path-finder 10 5 '(59 64 77 68 24 94 88 78 66 99 75 34 98 97 69 73 55 42 74 42 4 97 100 56 43 34 56 33 12 2 88 50 80 6 70 92 12 63 67 92 30 81 26 36 23 44 2 10 88 91))
      => '{:path (4 55 42 56 2), :total 155}]
     [matrix-ones (path-finder 10 3 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) =>  '{:path (0 1 1), :total 2}]
     ))