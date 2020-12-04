(ns puzzle-2 (:require hashp.core))

(def input (->> (slurp "input2.txt")))

(defn parse [[low high character password]]
  {:low (Integer/parseInt low)
   :high (Integer/parseInt high)
   :character (first character)
   :password password})

(defn valid-password? [{:keys [low high character password]}]
  (let [freq ((frequencies password) character 0)]
    (<= low freq high)))

(defn valid-positional-password? [{:keys [low high character password]}]
  (->> [low high]
       (map #(nth password (dec %)))
       (filter #{character})
       (count)
       (= 1)))

(defn puzzle1 [passwords]
  (->> passwords
    (re-seq #"(\d+)-(\d+) (\w): (\w+)")
    (map (comp parse rest))
    (filter valid-password?)
    (count)))

(defn puzzle2 [passwords]
  (->> passwords
    (re-seq #"(\d+)-(\d+) (\w): (\w+)")
    (map (comp parse rest))
    (filter valid-positional-password?)
    (count)))

(comment (time (puzzle1 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n")))
(comment (time (puzzle1 input)))

(comment (time (puzzle2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n")))
(comment (time (puzzle2 input)))
