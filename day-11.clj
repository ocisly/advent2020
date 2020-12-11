(ns day-11 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))
(def input (slurp "input11.txt"))

(defn parse [c]
  (identity c))

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

(defn neighbors [x y board]
  ((juxt
     north-west north north-east
     west             east
     south-west south south-east) x y board))

(defn parse-line [line]
  (mapv parse line))

(defn transform [i j cell board]
  (let [xs (frequencies (neighbors i j board))
        occupied (get xs \# 0)]
    (cond
      (and (= cell \L) (zero? occupied)) \#
      (and (= cell \#) (>= occupied 5)) \L
      :else cell)))

(defn reshuffle [board]
  (into []
        (for [[i row] (map-indexed vector board)]
           (into [] (map-indexed #(transform i %1 %2 board) row)))))

(defn pretty [board]
  (clojure.string/join "\n" (map clojure.string/join board)))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (mapv parse-line)
       (iterate reshuffle)
       (partition 2 1)
       (drop-while #(apply not= %))
       (first)
       (second)
       (apply concat)
       (frequencies)
       (#(% \#))))

(comment (puzzle2 input))
