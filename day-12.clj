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

(defn navigate1 [ship action]
  (if (= (:param action) :forward)
    (case (mod (:rotation ship) 360)
      0 (update ship :x + (:value action))
      90 (update ship :y - (:value action))
      180 (update ship :x - (:value action))
      270 (update ship :y + (:value action)))
    (update ship (:param action) + (:value action))))

(defn manhattan-distance [{:keys [x y]}]
  (+ (Math/abs x) (Math/abs y)))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (map parse)
       (reduce navigate1 {:x 0 :y 0 :rotation 0})
       (manhattan-distance)))

(defn rotate [waypoint angle]
  (let [angle (Math/toRadians (- angle))]
    {:x (int
          (Math/round
            (- (* (Math/cos angle) (:x waypoint))
               (* (Math/sin angle) (:y waypoint)))))
     :y (int
          (Math/round
            (+ (* (Math/sin angle) (:x waypoint))
               (* (Math/cos angle) (:y waypoint)))))}))

(defn forward [p1 p2 distance]
  (-> p1
      (update :x + (* distance (:x p2)))
      (update :y + (* distance (:y p2)))))

(defn navigate2 [state action]
  (case (:param action)
    :x (update-in state [:waypoint :x] + (:value action))
    :y (update-in state [:waypoint :y] + (:value action))
    :rotation (update state :waypoint rotate (:value action))
    :forward (update state :ship forward (:waypoint state) (:value action))))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (map parse)
       (reduce navigate2 {:ship {:x 0 :y 0} :waypoint {:x 10 :y 1}})
       (:ship)
       (manhattan-distance)))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))
