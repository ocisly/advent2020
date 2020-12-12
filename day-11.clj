(ns day-11 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))
(def input (slurp "input11.txt"))

(defn on-board? [x y board]
  (and (contains? board x)
       (contains? (board 0) y)))

(defn find-next-seat [x y dx dy board]
  (let [x (+ x dx) y (+ y dy)]
    (cond
      (not (on-board? x y board)) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y dx dy board))))

(defn neighbors2 [x y board]
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not= dx dy 0)]
    (find-next-seat x y dx dy board)))

(defn neighbors1 [x y board]
  (for [a (range -1 2) :let [i (+ x a)]
        b (range -1 2) :let [j (+ y b)]
        :when (and (not= a b 0) (on-board? i j board))]
    ((board i) j)))

(defn transform [cell k neighbors]
  (let [xs (frequencies neighbors)
        occupied (get xs \# 0)]
    (cond
      (and (= cell \L) (zero? occupied)) \#
      (and (= cell \#) (>= occupied k)) \L
      :else cell)))

(defn reshuffle [k neighbor-fn board]
  (let [indices (for [x (range (count board))
                      y (range (count (board 0)))]
                  [x y])
        evolve (fn [board [x y]]
                 (update-in board [x y]
                            transform k (neighbor-fn x y board)))]
    (reduce evolve board indices)))

(defn find-fixed-point [f xs]
  (->> (iterate f xs)
       (partition 2 1)
       (drop-while #(apply not= %))
       (first)
       (second)))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (mapv vec)
       (find-fixed-point (partial reshuffle 4 neighbors1))
       (flatten)
       (frequencies)
       (#(% \#))))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (mapv vec)
       (find-fixed-point (partial reshuffle 5 neighbors2))
       (flatten)
       (frequencies)
       (#(% \#))))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))
