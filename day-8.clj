(ns day-8
  (:require [hashp.core]))

(def parse-int #(Integer/parseInt %))

(defn parse [instruction]
  (->> (clojure.string/split instruction #" ")
       ((fn [[instr offset]]
          {:instr (keyword instr)
           :offset (parse-int offset)}))))

(defn next-i [i {:keys [instr offset]}]
  (case instr
    :nop (inc i)
    :acc (inc i)
    :jmp (+ i offset)))

(defn next-acc [acc {:keys [instr offset]}]
  (case instr
    :acc (+ acc offset)
    acc))

(defn execute [i acc visited? instructions]
  (let [instr (instructions i)]
    (if (visited? i)
      acc
      (recur (next-i i instr) (next-acc acc instr) (conj visited? i) instructions))))
    
(defn puzzle1 [program]
  (->> (clojure.string/split-lines program)
       (map parse)
       (vec)
       (execute 0 0 #{})))

(comment (def input (->> (slurp "input8.txt"))))
(comment (def input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"))
(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))
