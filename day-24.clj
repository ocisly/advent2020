(ns day-24
  (:require [hashp.core]))

(def parse-int #(Long/parseLong %))

(defn parse [line]
  (->> (re-seq #"se|sw|nw|ne|e|w" line)
       (map keyword)))

(def delta {:e  {:x +1 :y -1 :z  0}
            :ne {:x +1 :y  0 :z -1}
            :nw {:x 0  :y +1 :z -1} 
            :w  {:x -1 :y +1 :z  0}
            :sw {:x -1 :y  0 :z +1}
            :se {:x  0 :y -1 :z +1}})

(defn traverse [lobby d]
  (merge-with + lobby d))

(defn find-tile [directions]
  (->> directions
       (map delta)
       (reduce traverse {:x 0 :y 0 :z 0})))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (map parse)
       (map find-tile)
       (frequencies)
       (vals)
       (map #(mod % 2))
       (map {1 :black 0 :white})
       (frequencies)
       (:black)))

(defn neighbors [place sparse]
  (for [d (vals delta)
        :let [cube (traverse place d)
              s (sparse cube)]
        :when s]
    s))

(defn transform [place new-sparse sparse]
  (let [nx (neighbors place sparse)
        active-neighbors (count nx)
        was-active (contains? sparse place)]
    (if (or (and was-active (#{1 2} active-neighbors))
            (and (not was-active) (#{2} active-neighbors)))
      (conj new-sparse place)
      (disj new-sparse place))))

(defn evolve [sparse]
  (let [[min-x max-x] (apply (juxt min max) (map :x sparse))
        [min-y max-y] (apply (juxt min max) (map :y sparse))
        [min-z max-z] (apply (juxt min max) (map :z sparse))
        indices (for [x (range (dec min-x) (inc (inc max-x)))
                      y (range (dec min-y) (inc (inc max-y)))
                      z (range (dec min-z) (inc (inc max-z)))
                      :when (zero? (+ x y z))]
                    {:x x :y y :z z})
        evolve' (fn [new-sparse place]
                  (transform place new-sparse sparse))]
    (reduce evolve' sparse indices)))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (map parse)
       (map find-tile)
       (frequencies)
       (filter (comp odd? val))
       (keys)
       (set)
       (iterate evolve)
       (drop 100)
       (first)
       (count)))

(def input (->> (slurp "input24.txt")))
(time (puzzle1 input))
(time (puzzle2 input))
