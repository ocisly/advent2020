(ns day-19 (:require [hashp.core]
                     [clojure.edn :as edn]))

(def parse-int #(Long/parseLong %))

(defn find-subrules [& xs]
  (->> xs
       (partition-by #{'|})
       (remove #{'(|)})
       (set)))

(defn build-rule [fst snd & rst]
  (cond
    (string? snd) {:id fst :literal (first snd)}
    (number? snd) {:id fst :subrules (apply find-subrules snd rst)}))

(defn parse-rule [line]
  (->> line
       (re-seq #"\d+|[|]|\"\w\"")
       (map clojure.edn/read-string)
       (apply build-rule)))

(defn consolidate [rules]
  (zipmap (map :id rules) rules))

(defn parse [[id & ids] rules text]
  (let [{:keys [literal subrules]} (rules id)]
    (cond
      (some? literal) (when (= literal (first text))
                        (recur ids rules (rest text)))
      (some? subrules) (some #(parse (concat % ids) rules text) subrules)
      (empty? text) :success)))

(defn solve [rules messages]
  (let [rules  (->> rules (map parse-rule) (consolidate))]
    (->> messages
         (filter #(parse (list 0) rules %))
         count)))

(defn puzzle [in]
  (->> (clojure.string/split-lines in)
       (partition-by empty?)
       (remove #{'("")})
       (apply solve)))

(def input1 (slurp "input19-1.txt"))
(def input2 (slurp "input19-2.txt"))
(time (puzzle input1))
(time (puzzle input2))
