(ns day-8 (:require [hashp.core]))

(def parse-int #(Integer/parseInt %))

(defn two-sum [sum numbers]
  (let [complements (map #(- sum %) numbers)
        complement? (set complements)]
    (take 2 (filter complement? numbers))))

(defn eq-sum-of-preamble [d k nums]
  (let [[preamble [fst & rst]] (split-at k (drop d nums))]
    (if (seq (two-sum fst preamble))
      (recur (inc d) k nums)
      fst)))

(defn puzzle1 [k input]
  (->> (clojure.string/split-lines input)
       (map parse-int)
       (eq-sum-of-preamble 0 k)))

(comment (def input (->> (slurp "input9.txt"))))
(comment (time (puzzle1 25 input)))

(comment (def input "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"))
(comment (time (puzzle1 5 input)))
