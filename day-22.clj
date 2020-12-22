(ns day-22
  (:require [hashp.core]))

(def parse-int #(Integer/parseInt %))

(defn play [deck1 deck2]
  (cond (empty? deck1) {:winner 2 :deck deck2}
        (empty? deck2) {:winner 1 :deck deck1}
        (> (peek deck1) (peek deck2)) (recur (conj (pop deck1)
                                                   (peek deck1)
                                                   (peek deck2))
                                             (pop deck2))
        (< (peek deck1) (peek deck2)) (recur (pop deck1)
                                             (conj (pop deck2)
                                                   (peek deck2)
                                                   (peek deck1)))))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (partition-by empty?)
       (map rest)
       (remove empty?)
       (map #(map parse-int %))
       (map #(into clojure.lang.PersistentQueue/EMPTY %))
       (apply play)
       (:deck)
       (reverse)
       (map-indexed #(* (inc %1) %2))
       (apply +)))

(def input (->> (slurp "input22.txt")))
(time (puzzle1 input))
