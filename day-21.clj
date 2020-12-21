(ns day-7
  (:require [hashp.core]))

(def parse-int #(Integer/parseInt %))

(defn parse-ingredients [s]
  (->> (re-seq #"\w+" s)
       (partition-by #{"contains"})
       (remove #{'("contains")})
       (map set)
       (zipmap [:ingredients :allergens])))

(defn allergen->ingredients [{:keys [ingredients allergens]}]
  (zipmap allergens (repeat ingredients)))

(defn intersect [xs]
  (apply merge-with clojure.set/intersection (zipmap (mapcat keys xs) (mapcat vals xs)) xs))

(defn solve1 [ingredients]
  (let [all (->> ingredients (map :ingredients))
        union (apply clojure.set/union all)
        flat (apply concat all)]
    (->> ingredients
         (map allergen->ingredients)
         (intersect)
         (vals)
         (apply clojure.set/union)
         (clojure.set/difference union)
         (#(filter % flat))
         (count))))

(defn puzzle1 [input]
  (->> input
       clojure.string/split-lines
       (map parse-ingredients)
       (solve1)))

(def input (->> (slurp "input21.txt")))
(time (puzzle1 input))
