(ns day-17 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

(def active? #{\#})

(defn dense->sparse [board]
  (let [width (count (board 0))
        mid-x (quot width 2)
        height (count board)
        mid-y (quot height 2)]
    (set (for [x (range width) 
               y (range height)
               :when (active? ((board y) x))]
          {:x (- mid-x x)
           :y (- mid-y y)
           :z 0
           :w 0}))))

(defn neighbors [place sparse]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dw [-1 0 1]
        :let [cube {:x (+ (:x place) dx)
                    :y (+ (:y place) dy)
                    :z (+ (:z place) dz)
                    :w (+ (:w place) dw)}
              s (sparse cube)]
        :when (and (not= 0 dx dy dz dw) s)]
    s))

(defn transform [place new-sparse sparse]
  (let [nx (neighbors place sparse)
        active-neighbors (count nx)
        was-active (contains? sparse place)]
    (if (or (and was-active (#{2 3} active-neighbors))
            (and (not was-active) (#{3} active-neighbors)))
      (conj new-sparse place)
      (disj new-sparse place))))

(defn evolve [sparse]
  (let [[min-x max-x] (apply (juxt min max) (map :x sparse))
        [min-y max-y] (apply (juxt min max) (map :y sparse))
        [min-z max-z] (apply (juxt min max) (map :z sparse))
        [min-w max-w] (apply (juxt min max) (map :w sparse))
        indices (for [x (range (dec min-x) (inc (inc max-x)))
                      y (range (dec min-y) (inc (inc max-y)))
                      z (range (dec min-z) (inc (inc max-z)))
                      w (range (dec min-w) (inc (inc max-w)))]
                    {:x x :y y :z z :w w})
        evolve' (fn [new-sparse place]
                  (transform place new-sparse sparse))]
    (reduce evolve' sparse indices)))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (mapv vec)
       (dense->sparse)
       (iterate evolve)
       (#(nth % 6))
       (count)))

(def input (slurp "input17.txt"))
(time (puzzle2 input))
(comment (time (puzzle2 input)))
