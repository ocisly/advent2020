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
      \L {:param :rotation :value (- value)}
      \R {:param :rotation :value value}
      \F {:param :forward :value value})))

(defn navigate1 [ship {:keys [param value]}]
  (if (= param :forward)
    (case (mod (:rotation ship) 360)
      0 (update ship :x + value)
      90 (update ship :y - value)
      180 (update ship :x - value)
      270 (update ship :y + value))
    (update ship param + value)))

(defn manhattan-distance [{:keys [x y]}]
  (+ (Math/abs x) (Math/abs y)))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (map parse)
       (reduce navigate1 {:x 0 :y 0 :rotation 0})
       (manhattan-distance)))

(defn rotate [{:keys [x y]} angle]
  (case angle
    ( 90 -270) {:x y     :y (- x)}
    (180 -180) {:x (- x) :y (- y)}
    (270  -90) {:x (- y) :y x}))

(defn forward [p1 p2 distance]
  (-> p1
      (update :x + (* distance (:x p2)))
      (update :y + (* distance (:y p2)))))

(defn navigate2 [state {:keys [param value]}]
  (case param
    :x (update-in state [:waypoint :x] + value)
    :y (update-in state [:waypoint :y] + value)
    :rotation (update state :waypoint rotate value)
    :forward (update state :ship forward (:waypoint state) value)))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (map parse)
       (reduce navigate2 {:ship {:x 0 :y 0} :waypoint {:x 10 :y 1}})
       (:ship)
       (manhattan-distance)))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))
