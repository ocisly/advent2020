(ns day-10 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

(defn select-vals [keyseq m]
  (for [k keyseq] (m k)))

(defn puzzle1 [input]
  (->> (clojure.string/split-lines input)
       (map parse-int)
       (sort)
       (cons 0)
       (partition 2 1)
       (map reverse)
       (map (partial apply -))
       (cons 3)
       (frequencies)
       (select-vals [1 3])
       (apply *)))

(def input (slurp "input10.txt"))

(puzzle1 input)
