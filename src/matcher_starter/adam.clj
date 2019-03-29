(def matrix '(3 4 1 2 8 6
               6 1 8 2 7 4
               5 9 3 9 9 5
               8 4 1 3 2 6
               3 7 2 8 6 4
               ))

(defn make-matrix [row col matrix]
  (cond
    (= 0 row)
    nil
    :else
    (cons (take col matrix)
          (make-matrix (- row 1) col (drop col matrix)))))

(defn get-at [x y matrix]
  (nth (nth matrix y) x))

(defn get-cols [matrix]
  (cond
    (first (= () matrix))
    nil
    :else
    (cons (first matrix)
          (get-cols(rest matrix)))))




