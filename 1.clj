(def input (->> (slurp "input.txt")
                (clojure.string/split-lines)
                (map #(Integer/parseInt %))))

(defn sum-to [diffs number]
  (if-let [diff (diffs number)]
    (reduced [diff number])
    diffs))


(def diff-2020 (partial - 2020))

(defn puzzle1 [numbers]
  (let [diffs (zipmap (map diff-2020 numbers) numbers)
        [x y] (reduce sum-to diffs numbers)]
    (* x y)))

(defn debug [x]
  (prn x)
  x)

(defn puzzle2 [numbers]
  (let [n (count numbers)]
    (first
      (for [i (range n) :let [x (nth numbers i)]
            j (range (inc i) n) :let [y (nth numbers j)]
            k (range (inc j) n) :let [z (nth numbers k)]
            :when (= 2020 (+ x y z))]
        (* x y z)))))

(comment (time (puzzle1 input)))
(comment (time (puzzle2 [1721 979 366 299 675 1456])))
(comment (time (puzzle2 input)))
