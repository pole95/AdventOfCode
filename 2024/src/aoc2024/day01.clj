
(ns aoc2024.day01
  (:require [clojure.string :as str]))

(defn part1 [lines]
  (let [pairs (map #(str/split % #"   ") lines)
        lefts (sort (map #(Integer/parseInt (first %)) pairs))
        rights (sort (map #(Integer/parseInt (second %)) pairs))]
    (reduce + (map #(abs (- %1 %2)) lefts rights))))

(defn part2 [lines]
  (let [pairs (map #(str/split % #"   ") lines)
        lefts (sort (map #(Integer/parseInt (first %)) pairs))
        rights (sort (map #(Integer/parseInt (second %)) pairs))]
    (reduce + (map #(* % ((frequencies rights) % 0)) lefts))))

(defn -main [input]
  (let [result (part1 input)]
    (println "Part 1: " result))
  (let [result (part2 input)]
    (println "Part 2: " result)))