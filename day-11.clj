(ns day-11 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))
(def input (slurp "input11.txt"))

(defn west [x y board]
  (let [y (dec y)]
    (cond
      (< y 0) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn east [x y board]
  (let [y (inc y)]
    (cond
      (>= y (count (board 0))) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn north [x y board]
  (let [x (dec x)]
    (cond
      (< x 0) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn south [x y board]
  (let [x (inc x)]
    (cond
      (>= x (count board)) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn north-west [x y board]
  (let [x (dec x) y (dec y)]
    (cond
      (or (< x 0) (< y 0)) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn south-east [x y board]
  (let [x (inc x) y (inc y)]
    (cond
      (or (>= x (count board)) (>= y (count (board 0)))) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn north-east [x y board]
  (let [x (dec x) y (inc y)]
    (cond
      (or (< x 0) (>= y (count (board 0)))) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn south-west [x y board]
  (let [x (inc x) y (dec y)]
    (cond
      (or (>= x (count board)) (< y 0)) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y board))))

(defn neighbors2 [x y board]
  ((juxt
     north-west north north-east
     west             east
     south-west south south-east) x y board))

(defn neighbors1 [x y board]
  (for [a (range -1 2)
        :let [i (+ x a)]
        :when (< -1 i (count board))
        b (range -1 2)
        :let [j (+ y b)]
        :when (and (not= a b 0)
                   (< -1 j (count (board 0))))]
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
                            transform
                            k
                            (neighbor-fn x y board)))]
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
       (apply concat)
       (frequencies)
       (#(% \#))))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (mapv vec)
       (find-fixed-point (partial reshuffle 5 neighbors2))
       (apply concat)
       (frequencies)
       (#(% \#))))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))
