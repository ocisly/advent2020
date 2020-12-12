(ns day-12 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))
(def input (slurp "input12.txt"))

(defn parse [[action & value]]
  (let [value (parse-int (apply str value))]
    (case action
      \N {:param :y :value value}
      \S {:param :y :value (- value)}
      \E {:param :x :value value}
      \W {:param :x :value (- value)}
      \L {:param :heading :value (- value)}
      \R {:param :heading :value value}
      \F {:forward value})))

(defn navigate [ship action]
  (if-let [value (:forward action)]
    (case (mod (:heading ship) 360)
      0 (update ship :x + value)
      90 (update ship :y - value)
      180 (update ship :x - value)
      270 (update ship :y + value))
    (update ship (:param action) + (:value action))))

(defn manhattan-distance [{:keys [x y]}]
  (+ (Math/abs x) (Math/abs y)))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (map seq)
       (map parse)
       (reduce navigate {:x 0 :y 0 :heading 0})
       (manhattan-distance)))

(puzzle1 input)
