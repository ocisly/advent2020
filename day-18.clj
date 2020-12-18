(ns day-18 (:require [hashp.core]
                     [clojure.edn :as edn]))

(def parse-int #(Long/parseLong %))

(defn evaluate [stack x]
  ;(println "eval" x "stack" stack)
  (cond
    (sequential? x) (recur stack (first (reduce evaluate (list) x)))
    (and (number? x) (empty? stack)) (conj stack x)
    (number? x) (let [[op y & rst] stack]
                  (conj rst ((eval op) x y)))
    ('#{+ - * /} x) (conj stack x)))

(defn lex [expr]
  (edn/read-string (str "(" expr ")")))

(defn puzzle1 [in]
  (->> (clojure.string/split-lines in)
       (map lex)
       (map (partial reduce evaluate (list)))
       (map first)
       (apply +)))

(def input (slurp "input18.txt"))
(time (puzzle1 input))
