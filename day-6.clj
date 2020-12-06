(ns day-6 (:require hashp.core))

(def input (->> (slurp "input6.txt")))

(defn puzzle1 [in]
  (->> (clojure.string/split in #"\n\n")
       (map clojure.string/split-lines)
       (map clojure.string/join)
       (map set)
       (map count)
       (apply +)))

(defn puzzle2 [in]
  (->> (clojure.string/split in #"\n\n")
       (map clojure.string/split-lines)
       (map #(map set %))
       (map #(apply clojure.set/intersection %))
       (map count)
       (apply +)))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))
