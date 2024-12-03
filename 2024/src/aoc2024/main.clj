(ns aoc2024.main
  (:require [clojure.string :as str]
            [aoc2024.day01 :as day1]
            [aoc2024.day02 :as day2]
            [aoc2024.day03 :as day3]))

(defn readFile [day]
  (slurp (str "inputs/day" day ".txt")))

(defn -main []
  (println "Day 1")
  (let [start (System/nanoTime)]
    (day1/-main (str/split-lines (readFile "01")))
    (let [end (System/nanoTime)]
      (println "Took " (/ (- end start) 1e6) "ms")))
  (println "Day 2")
  (let [start (System/nanoTime)]
    (day2/-main (str/split-lines (readFile "02")))
    (let [end (System/nanoTime)]
      (println "Took " (/ (- end start) 1e6) "ms")))
    (println "Day 3")
  (let [start (System/nanoTime)]
    (day3/-main (readFile "03"))
    (let [end (System/nanoTime)]
      (println "Took " (/ (- end start) 1e6) "ms"))))