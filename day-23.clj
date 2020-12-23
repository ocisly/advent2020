(ns day-23
  (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

(defn find-dest [current reserved]
  (let [dest (dec current)]
    (cond
      (zero? dest) (recur 10 reserved)
      (reserved dest) (recur dest reserved)
      :else dest)))

(defn split-by [dest xs]
  (let [[before [_ & after]]
        (split-with (complement #{dest}) xs)]
    [before after]))

(defn play [[current & cups]]
  (let [[three others] (split-at 3 cups)
        dest (find-dest current (set three))
        [before after] (split-by dest others)]
    (concat before (cons dest three) after (list current))))

(def rotate-while
  (comp (partial apply concat) (juxt drop-while take-while)))

(defn puzzle1 [in]
  (->> in
       (re-seq #"\d")
       (map parse-int)
       (iterate play)
       (take 101)
       (last)
       (rotate-while (complement #{1}))
       (rest)
       (apply str)))
      

(def input (->> (slurp "input23.txt")))
(time (puzzle1 input))
