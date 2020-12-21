(ns day-20
  (:require [hashp.core]))

(def parse-int #(Integer/parseInt %))

(defn parse-tile [lines]
  (let [[title tile] (split-at 1 lines)]
    {:id (->> title first (re-find #"\d+") parse-int)
     :tile tile}))

(defn parse [input]
  (->> input
       (clojure.string/split-lines)
       (partition-by empty?)
       (remove #{'("")})
       (map parse-tile)))

(defn tile->node [{:keys [id tile]}]
  [{:id id
    :edge (first tile)}
   {:id id
    :edge (last tile)}
   {:id id
    :edge (apply str (map first tile))}
   {:id id
    :edge (apply str (map last tile))}])

(def gen-reverse
  (juxt identity #(update % :edge (comp (partial apply str) reverse))))

(defn edges->adj-list [acc [id id']]
  (-> acc
      (update id (fnil conj #{}) id')
      (update id' (fnil conj #{}) id)))

(defn puzzle1 [input]
  (->> input
       parse
       (mapcat tile->node)
       (mapcat gen-reverse)
       (group-by :edge)
       (vals)
       (map #(map :id %))
       (filter second)
       (reduce edges->adj-list {})
       (filter #(= (count (val %)) 2))
       (map key)
       (apply *)))

(def input (->> (slurp "input20.txt")))
(time (puzzle1 input))
