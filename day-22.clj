(ns day-22
  (:require [hashp.core]))

(def parse-int #(Integer/parseInt %))

(def queue (partial into clojure.lang.PersistentQueue/EMPTY))

(defn play1 [deck1 deck2]
  (let [head1 (peek deck1)
        head2 (peek deck2)
        rest1 (pop deck1)
        rest2 (pop deck2)]
    (cond (empty? deck1) {:winner 2 :deck deck2}
          (empty? deck2) {:winner 1 :deck deck1}
          (> head1 head2) (recur (conj rest1 head1 head2) rest2)
          (< head1 head2) (recur rest1 (conj rest2 head2 head1)))))

(defn play2 [seen deck1 deck2]
  (let [new-seen (conj seen [deck1 deck2])
        head1 (peek deck1)
        head2 (peek deck2)
        rest1 (pop deck1)
        rest2 (pop deck2)]
    (cond
      ; prevent infinite games
      (= seen new-seen) {:winner 1 :deck deck1}

      ; player who has all the cards wins
      (empty? deck1) {:winner 2 :deck deck2}
      (empty? deck2) {:winner 1 :deck deck1}

      ; recursive game determines winner of round
      (and (>= (count rest1) head1)
           (>= (count rest2) head2))
      (case
        (:winner (play2 #{}
                        (queue (take head1 rest1))
                        (queue (take head2 rest2))))
        1 (recur new-seen (conj rest1 head1 head2) rest2)
        2 (recur new-seen rest1 (conj rest2 head2 head1)))

      ; not enough cards, higher card wins
      (> head1 head2) (recur new-seen (conj rest1 head1 head2) rest2)
      (< head1 head2) (recur new-seen rest1 (conj rest2 head2 head1)))))

(defn parse [in]
  (->> (clojure.string/split-lines in)
       (partition-by empty?)
       (map rest)
       (remove empty?)
       (map #(map parse-int %))
       (map queue)))

(defn puzzle1 [in]
  (->> (parse in)
       (apply play1)
       (:deck)
       (reverse)
       (map-indexed #(* (inc %1) %2))
       (apply +)))

(defn puzzle2 [in]
  (->> (parse in)
       (apply play2 #{})
       (:deck)
       (reverse)
       (map-indexed #(* (inc %1) %2))
       (apply +)))

(def input (->> (slurp "input22.txt")))
(time (puzzle1 input))
(time (puzzle2 input))
