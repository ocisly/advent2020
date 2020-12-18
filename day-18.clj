(ns day-18 (:require [hashp.core]
                     [clojure.edn :as edn]))

(def parse-int #(Long/parseLong %))

(defn evaluate1 [stack x]
  (cond
    (sequential? x) (recur stack (first (reduce evaluate1 (list) x)))
    (and (number? x) (empty? stack)) (conj stack x)
    (number? x) (let [[op y & rst] stack]
                  (conj rst ((eval op) x y)))
    ('#{+ - * /} x) (conj stack x)))

(defn evaluate2 [stack x]
  (cond
    (sequential? x) (recur stack (->> x
                                      (reduce evaluate2 (list))
                                      (reduce evaluate1 (list))
                                      first))
    (and (number? x) (empty? stack)) (conj stack x)
    (number? x) (let [[op y & rst] stack]
                  (case op
                    + (conj rst ((eval op) x y))
                    * (conj rst y op x)))
    ('#{+ *} x) (conj stack x)))

(defn lex [expr]
  (edn/read-string (str "(" expr ")")))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (map lex)
       (map (partial reduce evaluate1 (list)))
       (map first)
       (apply +)))

(defn puzzle2 [in]
  (->> (clojure.string/split-lines in)
       (map lex)
       (map (partial reduce evaluate2 (list)))
       (map (partial reduce evaluate1 (list)))
       (map first)
       (apply +)))

(def input (slurp "input18.txt"))
(time (puzzle1 input))
(time (puzzle2 input))
