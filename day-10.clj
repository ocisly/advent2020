(ns day-10 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

(defn select-vals [keyseq m]
  (for [k keyseq] (m k)))

(defn diffs [joltages]
  (->> joltages
       (partition 2 1)
       (map (comp (partial apply -) reverse))))

(defn puzzle1 [input]
  (let [joltages (->> (clojure.string/split-lines input) (map parse-int) sort (cons 0) vec)
        joltages (conj joltages (+ (last joltages) 3))]
    (->> (diffs joltages)
         (frequencies)
         (select-vals [1 3])
         (apply *))))

; from https://programming-idioms.org/idiom/67/binomial-coefficient-n-choose-k/2781/clojure#
(defn binom [n k]
  (let [fact #(apply * (range 1 (inc %)))]
    (/ (fact n)
       (* (fact k) (fact (- n k))))))

(defn combos [min-k n]
  (let [all (int (.pow 2M n))
        too-short (apply + (for [k (range min-k (inc n))] (binom n k)))]
    (- all too-short)))

(defn puzzle2 [input]
  (let [joltages (->> (clojure.string/split-lines input) (map parse-int) sort (cons 0) vec)
        joltages (conj joltages (+ (last joltages) 3))]
    (->> (diffs joltages)
         (partition-by identity)
         (filter (comp #{1} first))
         (map count)
         (filter #(>= % 2))
         (map dec)
         (map (partial combos 3))
         (apply *))))

(def input (slurp "input10.txt"))

(comment (puzzle1 input))
(comment (puzzle2 input))
