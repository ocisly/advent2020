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
           :z 0}))))

(defn sparse->dense [sparse]
  (let [[min-x max-x] (apply (juxt min max) (map :x sparse))
        [min-y max-y] (apply (juxt min max) (map :y sparse))
        [min-z max-z] (apply (juxt min max) (map :z sparse))]
    (for [z (range min-z (inc max-z))]
      (into [] (for [y (reverse (range min-y (inc max-y)))]
                 (into [] (for [x (reverse (range min-x (inc max-x)))]
                            (if (sparse {:x x :y y :z z})
                              \#
                              \.))))))))

(comment (defn reshuffle [transform-fn board]
          (let [indices (for [x (range (count board))
                              y (range (count (board 0)))]
                          [x y])]
            (reduce evolve board indices))))

(defn neighbors [place sparse]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :let [cube {:x (+ (:x place) dx)
                    :y (+ (:y place) dy)
                    :z (+ (:z place) dz)}
              s (sparse cube)]
        :when (and (not= 0 dx dy dz) s)]
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
        indices (for [x (range (dec min-x) (inc (inc max-x)))
                      y (range (dec min-y) (inc (inc max-y)))
                      z (range (dec min-z) (inc (inc max-z)))]
                    {:x x :y y :z z})
        evolve' (fn [new-sparse place]
                  (transform place new-sparse sparse))]
    (reduce evolve' sparse indices)))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (mapv vec)
       (dense->sparse)
       (iterate evolve)
       (#(nth % 6))
       (count)))
       ;(sparse->dense)
       ;(map (partial map clojure.string/join))
       ;(map #(clojure.string/join "\n" %))
       ;(map #(do (println) (println %)))))

(def input (slurp "input17.txt"))
(time (puzzle1 input))
(comment (time (puzzle2 input)))
