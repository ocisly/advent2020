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

(defn matches-all-vals? [values rule]
  (every? #(matches? % rule) values))

(defn find-rules [i rules values]
  (->> rules
       (filter #(matches-all-vals? values %))
       (set)
       (assoc {:pos i} :rules)))

(defn find-rule [[r1 r2]]
  (->> (map :rules [r2 r1])
       (apply clojure.set/difference)
       (first)
       (assoc (select-keys r2 [:pos]) :rule)))

(defn rule-of-interest? [{{field :field} :rule}]
  (clojure.string/starts-with? field "departure"))

(defn solve2 [{:keys [rules yours others]}]
  (->> others
       (filter #(empty? (find-mismatches rules %)))
       (apply map hash-set)
       (map-indexed #(find-rules %1 rules %2))
       (sort-by (comp count :rules))
       (cons nil)
       (partition 2 1)
       (map find-rule)
       (filter rule-of-interest?)
       (map :pos)
       (map (vec yours))
       (apply *)))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (partition-by empty?)
       (remove #(= (count %) 1))
       (apply parse)
       (solve2)))

(def input (slurp "input16.txt"))
(time (puzzle1 input))
(time (puzzle2 input))
