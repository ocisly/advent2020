(ns day-7
  (:require [hashp.core]
            [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]))

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

(defn bags-contained-in [needle facts]
  (pldb/with-db facts
    (l/run* [q]
            (l/fresh [x y]
                     (can-contain-transitive x needle y)
                     (l/== q [x y])))))

(defn bags-that-contain [needle facts]
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

(defn puzzle1 [bag rules]
  (->> rules
       clojure.string/split-lines
       (map parse-rule)
       (mapcat expand-rule)
       (ingest)
       (bags-contained-in bag)
       (map first)
       (set)
       (count)))

(defn puzzle2 [bag rules]
  (->> rules
       clojure.string/split-lines
       (map parse-rule)
       (mapcat expand-rule)
       (ingest)
       (bags-that-contain bag)
       (map second)
       (apply +)))

(def input (->> (slurp "input7.txt")))
(comment (time (puzzle1 "shiny gold" input)))
(comment (time (puzzle2 "shiny gold" input)))
