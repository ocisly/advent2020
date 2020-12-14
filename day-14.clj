(ns day-13 (:require [hashp.core]))

(def parse-bin #(Long/parseLong % 2))
(def parse-int #(Long/parseLong %))

(defn parse [[instr value]]
  (case instr
    "mask" {:instr :mask :value value}
    {:instr :mem
     :index (parse-int (re-find #"\d+" instr))
     :value (parse-int value)}))

(defn mask-bit [m v]
  (case m
    \X #{\0 \1}
    \1 \1
    \0 v))

(defn store-indirect [mem dests value]
  (reduce #(assoc %1 %2 value) mem dests))

(defn bifurcate [head [bit & address]]
  (cond
    (nil? bit) (list head)
    (set? bit) (mapcat #(bifurcate (conj head %) address) bit)
    :else (recur (conj head bit) address)))

(defn apply-mask [mask value]
  (let [binary (Long/toString value 2)
        padding (repeat (- (count mask) (count binary)) \0)]
    (->> (clojure.string/join (concat padding binary))
         (map mask-bit mask)
         (bifurcate [])
         (map clojure.string/join)
         (map parse-bin))))

(defn execute [acc instr]
  (case (:instr instr)
    :mask (assoc acc :mask (:value instr))
    :mem (update acc :mem store-indirect (apply-mask (:mask acc) (:index instr)) (:value instr))))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines input)
       (map #(clojure.string/split % #" = "))
       (map parse)
       (reduce execute {:mem {}})
       (:mem)
       (vals)
       (apply +)))

(def input (slurp "input14.txt"))
(puzzle2 input)
