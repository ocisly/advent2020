(ns day-1 (:require hashp.core))

(def input (->> (slurp "input1.txt")
                (clojure.string/split-lines)
                (map #(Integer/parseInt %))))

(defn two-sum [sum numbers]
  (let [complements (map #(- sum %) numbers)
        complement? (set complements)]
    (take 2 (filter complement? numbers))))

(defn three-sum [sum [x & xs]]
  (let [diff (- sum x)
        pair (two-sum diff xs)]
    (if-let [[y z] (seq pair)]
      [x y z]
      (recur sum xs))))

(defn puzzle1 [numbers]
  (apply * (two-sum 2020 numbers)))

(defn puzzle2 [numbers]
  (apply * (three-sum 2020 numbers)))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 [1721 979 366 299 675 1456])))
(comment (time (puzzle2 input)))
