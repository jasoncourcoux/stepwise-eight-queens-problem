(ns stepwise-problem.after
  (:require [clojure.spec :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test :as stest]))

;;
;; If you say there are a set of candidates A where one is to be selected and satisfies condition p
;;
;; Solution x is:
;; x is a member of set A, and p(x) is true,
;;
;; Can be solved, by generating all A until we reach a valid solution or there are no more candidates.
;;
;; This is far too inefficient so we need to simplify the problem.
;;
;; To do this, we find a representation of p where
;;
;; p = q and r
;;
;; Then if we say Br is the set:
;; (x is a member of A) and r(x) is true
;;
;; Then, Br must be a subset of A and therefore we only need to generate the subset Br and test each against q
;;
;; So we need to find a function r which causes Br to be a significantly small set than A, that we can easily
;; generate Br and where q is easier to test against that p.

;; If we think about the problem statement:
;; Given an 8x8 chessboard and 8 queens which are hostile to each other. Find a position for each queen
;; (a configuration) such that no queen may be taken by any other queen (i.e. such that every row, column
;; and diagonal contains at most one queen).
;;
;; We can see that we can split the problem into 2 parts
;;  p = every column must have exactly one queen AND there must be at most one queen in every row and every diagonal.
;;
;; q is easier to test as we have one less condition to check and we have significantly reduced the set of possible
;; configurations
;;
;; We can do better though as this is still pretty inefficent. To do this, we try one step at a time and if we are
;; successful we advance, otherwise we regress and try the next starting step until we have found a solution.
;;
;; So in terms of the 8 queens problem. We try each empty cell in the first column, and then try to place a queen
;; successfully in the second column - if we can do, we attempt the third column but if we can't we know that the
;; first placement is invalid and move it onto the next available cell in the column. We repeat, until a solution
;; has been found.
;;
;;

;; Borrow some of the utility functions/variables from my original attempt:

(def board-size 10) ;; Upped to 10 which shows a clearer performance difference on my machine
(def queen "Q")
(def empty-cell " ")
(def blocked-cell "X")

(defn diagonal-coordinates [col row]
  "Generates the diagonal coordinates from the given position and within
  the board boundaries."
  (into [] (concat
             (map (fn [x y] [x y])
                  (take-while #(>= % 0) (iterate dec col))
                  (take-while #(>= % 0) (iterate dec row)))
             (map (fn [x y] [x y])
                  (take-while #(> board-size %) (iterate inc col))
                  (take-while #(>= % 0) (iterate dec row)))
             (map (fn [x y] [x y])
                  (take-while #(> board-size %) (iterate inc col))
                  (take-while #(> board-size %) (iterate inc row)))
             (map (fn [x y] [x y])
                  (take-while #(>= % 0) (iterate dec col))
                  (take-while #(> board-size %) (iterate inc row))))))

(defn set-queen [board [col row]]
  "Takes a board and coordinates, and returns a new board with the queen at those coordinates and
   any spaces that will now be blocked/guarded by the positioned queen.

   No validation is performed and so may return invalid boards"
  (as-> board b
        (mapv #(assoc % col blocked-cell) b)
        (assoc-in b [row] (into [] (repeat board-size blocked-cell)))
        (reduce (fn [nb [c r]] (assoc-in nb [r c] blocked-cell)) b (diagonal-coordinates col row))
        (assoc-in b [row col] queen)))


(defn configurations
  "This is the function that applies the step logic and returns the first available solution it finds."
  ([]
   (let [board (vec (repeat board-size (vec (repeat board-size empty-cell))))]
     (configurations board 0 0)))
  ([board col row]
   (cond (>= row board-size) nil
         (>= col board-size) (if (= board-size (count (filter #{queen} (flatten board)))) board nil)
         ((comp not #{empty-cell}) (get-in board [row col])) (configurations board col (inc row))
         (nil? (configurations (set-queen board [col row]) (inc col) 0)) (configurations board col (inc row))
         :else (configurations (set-queen board [col row]) (inc col) 0))))

(time (configurations))

;; "Elapsed time: 206.518946 msecs"

