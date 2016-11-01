(ns stepwise-problem.before
  (:require [clojure.spec :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test :as stest]))

(def board-size 10) ;; Upped to 10 which shows a clearer performance difference on my machine

(def queen "Q")
(def empty-cell " ")
(def blocked-cell "X")

(s/def ::cell #{queen empty-cell blocked-cell})
(s/def ::row (s/coll-of ::cell :count board-size))
(s/def ::board (s/coll-of ::row :count board-size))

(s/def ::empty-board (s/coll-of
                       (s/coll-of #{empty-cell} :count board-size)
                       :count board-size))

(s/exercise ::board)

(s/exercise ::empty-board)

;; Strategy:
;; For each cell in row one, place a queen and mark the cells which are now blocked
;;
;; Then for each of the above configurations repeat on the second row for each valid position
;; with the second queen.
;;
;; Repeat for each row.

(s/def ::coordinates (s/tuple (s/int-in 0 8) (s/int-in 0 8)))

(s/exercise ::coordinates)

(s/fdef set-queen :args (s/cat :board ::empty-board
                               :coordinates ::coordinates)
        :ret ::board)

(s/exercise (:args (s/get-spec `set-queen)))

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

(s/exercise-fn `set-queen)

(s/fdef configurations
        :args (s/cat :board ::empty-board)
        :ret coll?)

(defn nqueens? [board n]
  "Predicate which checks that the number of queens on the given board is equal to n."
  (let [nq (count (filter #{queen} (flatten board)))]
    (>= nq n)))

(defn configurations
  "Gets the configurations for a board."
  ([board] (configurations board 0))
  ([board row]
   (if (= row 8)
     board
     (filter #(nqueens? % (inc row))
             (for [c (range board-size)]
               (if (= empty-cell (get-in board [row c]))
                 (configurations (set-queen board [c row]) (inc row))))))))

(defn all-configurations []
  "Flattens the result - should handle this in the configurations function, but will come back to later as
  this isn't the problem I'm trying to solve!"
  (vec (partition
         board-size
         (vec (partition
                board-size
                (flatten (configurations (vec (repeat board-size
                                                          (vec (repeat board-size empty-cell)))))))))))

(time (first (all-configurations)))

;;"Elapsed time: 13152.369771 msecs"
