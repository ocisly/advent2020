(ns day-13 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

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

(defn parse-bus [index string]
  (when (not= string "x")
    {:index index :id (parse-int string)}))

; For some reason, Math/abs does not work properly
(defn abs [n]
  (if (neg? n) (- n) n))

; from https://rosettacode.org/wiki/Modular_inverse#Clojure
(defn extended-gcd [a b]
  (cond (zero? a) [(abs b) 0 1]
        (zero? b) [(abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (abs b)
                     r0 (abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

; from https://rosettacode.org/wiki/Modular_inverse#Clojure
(defn mul-inv [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
      (when (= (first egcd) 1)
        (mod (second egcd) b))))

; adapted from https://rosettacode.org/wiki/Chinese_remainder_theorem#Python
(defn chinese-remainder [n a]
  (let [prod (apply * n)
        terms (for [[x y] (map vector n a)
                    :let [p (quot prod x)]]
                  (* y (mul-inv p  x) p))]
    (mod (apply + terms) prod)))

(defn puzzle2 [in]
  (let [[dep buses](clojure.string/split-lines in)
        dep (parse-int dep)
        buses (map-indexed parse-bus (clojure.string/split buses #","))]
    (->> (filter some? buses)
         (map (juxt :id #(mod (- (:id %) (:index %)) (:id %))))
         (apply map vector)
         (apply chinese-remainder))))
        
(def input (slurp "input13.txt"))
(puzzle1 input)
(puzzle2 input)
