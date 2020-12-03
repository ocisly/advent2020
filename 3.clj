(defn debug [& x] (apply prn x) x)

(def input (->> (slurp "input3.txt")))

(defn tree-at? [n line]
  (-> line (nth n) #{\#}))
  

(defn find-trees [right down lines]
  (map-indexed #(tree-at? (* %1 right) %2) (take-nth down lines)))

(defn puzzle1 [in]
  (->> in
       clojure.string/split-lines
       (map cycle)
       (find-trees 3 1)
       (filter some?)
       (count)))

(defn count-all-trees [lines]
  (for [[right down] [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (->> (find-trees right down lines)
         (filter some?)
         (count))))

(defn puzzle2 [in]
  (->> in
       clojure.string/split-lines
       (map cycle)
       count-all-trees
       (apply *)))

(comment (time (puzzle1 "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")))
(comment (time (puzzle1 input)))

(comment (time (puzzle2 "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")))
(comment (time (puzzle2 input)))
