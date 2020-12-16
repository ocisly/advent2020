(ns day-15 (:require [hashp.core]
                     [clojure.core.async :as async]))

(defn ring-buffer [size]
  {:i 0 :storage (mapv identity (repeat size nil))})

(defn ring-conj [buf x]
  (-> buf
      (update :storage assoc (:i buf) x)
      (update :i inc)
      (update :i mod (count (:storage buf)))))

(defn ring-seq [{:keys [i storage]}]
  (when storage
    (concat (drop i storage) (take i storage))))

(def parse-int #(Long/parseLong %))

(defn update-seen [buf i]
  (if (nil? buf)
    (recur (ring-buffer 2) i)
    (ring-conj buf i)))

(defn speak [previous index current history]
  (let [buf (history previous)
        [a b] (ring-seq buf)]
    (cond
      (< index 6) current
      (and a b) (- b a)
      :else 0)))

(defn last-spoken [acc [i n]]
  (let [new-acc (update acc :spoken speak i n (:when acc))]
    (update-in new-acc [:when (:spoken new-acc)] update-seen i)))

(defn puzzle1 [in]
  (->> (clojure.string/split in #",")
       (map parse-int)
       (cycle)
       (map-indexed vector)
       (reductions last-spoken {:when {}})
       (drop 30000000)
       (take 1)
       (map :spoken)))

(def input (slurp "input15.txt"))
(comment (def input "1,3,2"))

(puzzle1 (clojure.string/trim-newline input))
(comment
  (let [buf (ring-buffer 2)]
   (-> buf
     (ring-conj 5)
     (ring-conj 6)
     (ring-seq))))
