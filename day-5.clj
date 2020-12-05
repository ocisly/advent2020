(ns day-5 (:require hashp.core))

(def input (->> (slurp "input5.txt")))

(defn find-pos [low high [x & xs]]
  (if (= low high) high
    (let [mid (+ low (/ (- high low) 2))]
      (cond
        (#{\L \F} x) (recur low (int (Math/floor mid)) xs)
        (#{\R \B} x) (recur (int (Math/ceil mid)) high xs)))))

(def find-row (partial find-pos 0 127))
(def find-col (partial find-pos 0 7))

(defn find-seat [instructions]
  (let [[rows cols] (partition-all 7 instructions)
        row (find-row rows)
        col (find-col cols)]
    (+ (* row 8) col)))

(defn puzzle1 [boarding-passes]
  (->> boarding-passes
       (clojure.string/split-lines)
       (map find-seat)
       (apply max)))

(defn missing-seat? [[before after]]
  (= 2 (- after before)))

(defn puzzle2 [boarding-passes]
  (->> boarding-passes
       (clojure.string/split-lines)
       (map find-seat)
       (sort)
       (partition-all 2)
       (filter missing-seat?)))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))

(comment (time (find-seat "BBFFBBFRLL"))) ; 820
(comment (time (find-row "BBFFBBF"))) ; 102
(comment (time (find-col "RLL"))) ; 4

(comment (time (find-row "FBFBBFF"))) ; 44
(comment (time (find-col "RLR"))) ; 5

(comment (time (find-seat "BFFFBBFRRR"))) ; 567
(comment (time (find-row "BFFFBBF"))) ; 70
(comment (time (find-col "RRR"))) ; 7

(comment (time (find-seat "FFFBBBFRRR"))) ; 119
(comment (time (find-row "FFFBBBF"))) ; 14
(comment (time (find-col "RRR"))) ; 7
