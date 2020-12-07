(ns day-7
  (:require [hashp.core]
            [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))
   

(def input (->> (slurp "input7.txt")))
(comment (def input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))

(def parse-int #(Integer/parseInt %))

(defn parse-spec [spec]
  (->> (clojure.string/split spec #" " 2)
       ((juxt second (comp parse-int first)))))

(defn parse-rule [string]
  (let [[head body] (clojure.string/split string #" bags contain ")
        specs (clojure.string/split body #" bags?[,.] ?")] 
    {:container head
     :containees (into {} (map parse-spec (remove #{"no other"} specs)))}))
        
(pldb/db-rel can-contain ^:index p1 ^:index p2)


(defn can-contain-transitive [x y]
  (l/conde
    [(can-contain x y)]
    [(l/fresh [z]
      (can-contain x z)
      (can-contain-transitive z y))]))

(defn query [needle facts]
  (pldb/with-db facts
    (l/run* [q]
      (can-contain-transitive q needle))))

(defn expand-rule [{:keys [container containees]}]
  (map vector (repeat container) (map key containees)))

(defn ingest-rule [db [container containee]]
  (pldb/db-fact db can-contain container containee))

(defn ingest [rules]
  (reduce ingest-rule (pldb/db) rules))

(defn puzzle1 [bag rules]
  (->> rules
       clojure.string/split-lines
       (map parse-rule)
       (mapcat expand-rule)
       (ingest)
       (query bag)
       (set)
       (count)))

(comment (time (puzzle1 "shiny gold" input)))
