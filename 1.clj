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

(puzzle1 input)
