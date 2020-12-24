(ns day-24
  (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

(defn parse [line]
  (->> (re-seq #"se|sw|nw|ne|e|w" line)
       (map keyword)))

(def delta {:e  {:x +1 :y -1 :z  0}
            :ne {:x +1 :y  0 :z -1}
            :nw {:x 0  :y +1 :z -1} 
            :w  {:x -1 :y +1 :z  0}
            :sw {:x -1 :y  0 :z +1}
            :se {:x  0 :y -1 :z +1}})

(defn traverse [lobby d]
  (merge-with + lobby d))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (map parse)
       (map (partial map delta))
       (map (partial reduce traverse {:x 0 :y 0 :z 0}))
       (frequencies)
       (vals)
       (map #(mod % 2))
       (map {1 :black 0 :white})
       (frequencies)))

(def input (->> (slurp "input24.txt")))
(time (puzzle1 input))
