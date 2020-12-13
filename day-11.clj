(ns day-11 (:require [hashp.core]))

(def parse-int #(Long/parseLong %))
(def input (slurp "input11.txt"))

(defn on-board? [x y board]
  (and (contains? board x)
       (contains? (board 0) y)))

(defn find-next-seat [x y dx dy board]
  (let [x (+ x dx) y (+ y dy)]
    (cond
      (not (on-board? x y board)) nil
      (not= ((board x) y) \.) ((board x) y)
      :else (recur x y dx dy board))))

(defn neighbors2 [x y board]
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not= dx dy 0)]
    (find-next-seat x y dx dy board)))

(defn neighbors1 [x y board]
  (for [dx (range -1 2) :let [i (+ x dx)]
        dy (range -1 2) :let [j (+ y dy)]
        :when (and (not= dx dy 0) (on-board? i j board))]
    ((board i) j)))

(defn at-least [k pred xs]
  (>= (count (take k (filter pred xs))) k))

(def occupied? #{\#})

(defn transform [k cell neighbors]
  (case cell
    \. cell
    \L (if (not (at-least 1 occupied? neighbors)) \# cell)
    \# (if (at-least k occupied? neighbors) \L cell)))

(defn transform1 [cell x y board]
  (->> (neighbors1 x y board)
       (transform 4 cell)))

(defn transform2 [cell x y board]
  (->> (neighbors2 x y board)
       (transform 5 cell)))

(defn reshuffle [transform-fn board]
  (let [indices (for [x (range (count board))
                      y (range (count (board 0)))]
                  [x y])
        evolve (fn [new-board [x y]]
                 (update-in new-board [x y]
                            transform-fn x y board))]
    (reduce evolve board indices)))

(defn find-fixed-point [f xs]
  (->> (iterate f xs)
       (partition 2 1)
       (drop-while #(apply not= %))
       (first)
       (second)))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (mapv vec)
       (find-fixed-point #(reshuffle transform1 %))
       (flatten)
       (filter occupied?)
       (count)))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (mapv vec)
       (find-fixed-point #(reshuffle transform2 %))
       (flatten)
       (filter occupied?)
       (count)))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 input)))
