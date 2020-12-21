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

(defn find-ingredient [acc remaining [x & xs]]
  (let [seen (set (keys acc))
        allergen (:allergen x)
        [i & is] (clojure.set/difference (:ingredients x) seen)]
    (cond
      (and (nil? x) (empty? remaining)) acc
      (nil? x) (recur acc '() remaining)
      (empty? is) (recur (assoc acc i allergen) remaining xs)
      :else (recur acc (conj remaining x) xs))))

(defn solve2 [ingredients]
  (->> ingredients
       (map allergen->ingredients)
       (intersect)
       (map #(zipmap [:allergen :ingredients] %))
       (sort-by (comp count :ingredients))
       (find-ingredient {} '())
       (map #(zipmap [:ingredient :allergen] %))
       (sort-by :allergen)
       (map :ingredient)
       (clojure.string/join ",")))

(defn puzzle2 [input]
  (->> input
       clojure.string/split-lines
       (map parse-ingredients)
       (solve2)))

(def input (->> (slurp "input21.txt")))
(time (puzzle1 input))
(time (puzzle2 input))
