(ns day-16 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

(defn parse-rule [rule]
  (let [parts (re-find #"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)" rule)
        [field & nums] (rest parts)
        [lo1 hi1 lo2 hi2] (map parse-int nums)]
    {:field field
     :range1 {:low lo1 :high hi1}
     :range2 {:low lo2 :high hi2}}))

(defn parse-ticket [ticket]
  (->> (re-seq #"\d+" ticket)
       (map parse-int)))

(defn parse [rules yours others]
 {:rules (map parse-rule rules)
  :yours (parse-ticket (second yours))
  :others (map parse-ticket (rest others))})

(defn in-range? [value {:keys [low high]}]
  (when (<= low value high)
    value))

(defn matches? [value {:keys [range1 range2]}]
  (or (in-range? value range1) (in-range? value range2)))

(defn matches-none? [value rules]
  (not-any? #(matches? value %) rules))

(defn find-mismatches [rules values]
  (filter #(matches-none? % rules) values))

(defn solve1 [{:keys [rules others]}]
  (->> others
       (mapcat #(find-mismatches rules %))
       (apply +)))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (partition-by empty?)
       (remove #(= (count %) 1))
       (apply parse)
       (solve1)))

(def input (slurp "input16.txt"))
(comment (def input "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
"))

(puzzle1 input)
