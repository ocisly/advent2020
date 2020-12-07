(ns day-7
  (:require [hashp.core]
            [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]))
   

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
        
(pldb/db-rel can-contain ^:index p1 ^:index p2 n)


(defn can-contain-transitive [x y n]
  (l/conde
    [(can-contain x y n)]
    [(l/fresh [z o p]
              (can-contain x z o)
              (can-contain-transitive z y p)
              (fd/* o p n))]))

(defn query [needle facts]
  (pldb/with-db facts
    (l/run* [q]
      (l/fresh [x y]
        (can-contain-transitive needle x y)
        (l/== q [x y])))))

(defn expand-rule [{:keys [container containees]}]
  (map vector (repeat container) containees))

(defn ingest-rule [db [container containee]]
  (apply pldb/db-fact db can-contain container containee))

(defn ingest [rules]
  (reduce ingest-rule (pldb/db) rules))

(defn puzzle2 [bag rules]
  (->> rules
       clojure.string/split-lines
       (map parse-rule)
       (mapcat expand-rule)
       (ingest)
       (query bag)
       (map second)
       (apply +)))

(comment (time (puzzle2 "shiny gold" input)))
