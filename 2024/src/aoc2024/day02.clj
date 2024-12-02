(ns aoc2024.day02
  (:require [clojure.string :as str]))

(defn adjacentDiff? [xs]
  (every? #(and (>= % 1) (<= % 3)) (map #(abs (- (first %) (last %))) (partition 2 1 xs))))

(defn isOrdered? [xs]
  (or (apply > xs) (apply < xs)))

(defn isSafe? [xs]
  (and (adjacentDiff? xs) (isOrdered? xs)))

(defn listToInts [lst]
  (map #(Integer/parseInt %) lst))

(defn removeEach [xs]
  (map-indexed (fn [i _] (concat (take i xs) (drop (inc i) xs))) xs))

(defn part1 [input]
  (let [reports (map #(str/split % #" ") input)
        xs (map listToInts reports)]
    (count (filter isSafe? xs))))

(defn part2 [input]
  (let [reports (map #(str/split % #" ") input)
        xs (map listToInts reports)]
    (count (filter #(some isSafe? (removeEach %)) xs))))


(defn -main [input]
  (let [result (part1 input)]
    (println "Part 1: " result))
  (let [result (part2 input)]
    (println "Part 2: " result)))