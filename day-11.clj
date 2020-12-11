(ns day-11 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))
(def input (slurp "input11.txt"))

(defn parse [c]
  (identity c))

(defn neighbors [x y board]
  (for [a (range -1 2)
        :let [i (+ x a)]
        :when (< -1 i (count board))
        b (range -1 2)
        :let [j (+ y b)]
        :when (and (not= a b 0) 
                   (< -1 j (count (board 0))))]
    ((board i) j)))

(defn parse-line [line]
  (mapv parse line))

(defn transform [i j cell board]
  (let [xs (frequencies (neighbors i j board))
        occupied (get xs \# 0)]
    (cond
      (and (= cell \L) (zero? occupied)) \#
      (and (= cell \#) (>= occupied 4)) \L
      :else cell)))

(defn reshuffle [board]
  (into []
        (for [[i row] (map-indexed vector board)]
           (into [] (map-indexed #(transform i %1 %2 board) row)))))

(defn pretty [board]
  (clojure.string/join "\n" (map clojure.string/join board)))

(defn puzzle1 [in]
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

(puzzle1 input)
(comment (puzzle2 input))
