(ns day-8 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

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

(defn find-range-sum-to [secret-num nums]
  (loop [i 0 j 0 sum 0]
    (cond
      (= sum secret-num) (map nums (range i j))
      (> sum secret-num) (recur (inc i) (inc i) 0)
      (< sum secret-num) (recur i       (inc j) (+ sum (nums j))))))

(defn puzzle2 [k input]
  (let [nums (->> (clojure.string/split-lines input)
                  (map parse-int)
                  vec)
        secret-num (eq-sum-of-preamble 0 k nums)]
    (->> (find-range-sum-to secret-num nums)
         (apply (juxt min max))
         (apply +))))

(comment (def input (->> (slurp "input9.txt"))))
(comment (time (puzzle1 25 input)))
(comment (time (puzzle2 25 input)))

(comment (def input "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"))
(comment (time (puzzle1 5 input)))
(comment (time (puzzle2 5 input)))
