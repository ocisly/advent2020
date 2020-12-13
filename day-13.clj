(ns day-13 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))
(def input (slurp "input13.txt"))
(comment (def input "939
7,13,x,x,59,x,31,19"))

(defn departure-times [bus]
  {:id bus :times (reductions + 0 (repeat bus))})

(defn departing-after [dep bus]
  (assoc bus :earliest (first (drop-while #(< % dep) (:times bus)))))

(defn puzzle1 [in]
  (let [[dep buses](clojure.string/split-lines in)
        dep (parse-int dep)
        buses (map parse-int (remove #{"x"} (clojure.string/split buses #",")))]
    (->> buses
         (map departure-times)
         (map #(departing-after dep %))
         (apply min-key :earliest)
         (#(* (:id %) (- (:earliest %) dep))))))

(puzzle1 input)
