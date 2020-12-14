(ns day-13 (:require [hashp.core]))

(def parse-bin #(Long/parseLong % 2))
(def parse-int #(Long/parseLong %))
(def input (slurp "input14.txt"))
(comment (def input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"))

(defn parse [[instr value]]
  (case instr
    "mask" {:instr :mask :value value}
    {:instr :mem
     :index (parse-int (re-find #"\d+" instr))
     :value (parse-int value)}))

(defn mask-bit [m v]
  (case m
    \X v
    \1 \1
    \0 \0))
    
(defn apply-mask [mask value]
  (let [binary (Long/toString value 2)
        padding (repeat (- (count mask) (count binary)) \0)]
    (->> (clojure.string/join (concat padding binary))
         (map mask-bit mask)
         (clojure.string/join)
         (parse-bin))))

(defn execute [acc instr]
  (case (:instr instr)
    :mask (assoc acc :mask (:value instr))
    :mem (assoc-in acc [:mem (:index instr)] (apply-mask (:mask acc) (:value instr)))))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines input)
       (map #(clojure.string/split % #" = "))
       (map parse)
       (reduce execute {:mem {}})
       (:mem)
       (vals)
       (apply +)))

(puzzle1 input)
