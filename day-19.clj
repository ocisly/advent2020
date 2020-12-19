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

(defn build-regex [id rules]
  (let [{:keys [literal subrules]} (rules id)]
    (cond
      (some? literal) literal
      (some? subrules)
      (let [sub (for [ids subrules]
                    (apply str (map #(build-regex % rules) ids)))
            sub (clojure.string/join "|" sub)]
        (str "(" sub ")")))))


(defn build-pattern [rules]
  (->> (map parse-rule rules)
       (consolidate)
       (build-regex 0)
       (#(str "^" % "$"))
       (re-pattern)))

(defn solve [rules messages]
  (let [pattern (build-pattern rules)]
    (->> messages
         (filter #(re-find pattern %))
         (count))))


(defn puzzle1 [in] 
  (->> (clojure.string/split-lines in)
       (partition-by empty?)
       (remove #{'("")})
       (apply solve)))

(def input (slurp "input19.txt"))
(time (puzzle1 input))
