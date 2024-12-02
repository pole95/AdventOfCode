(ns aoc2024.main
  (:require [clojure.string :as str]
            [aoc2024.day01 :as day1]
            [aoc2024.day02 :as day2]))

(defn readFile [day]
  (let [content (slurp (str "inputs/day" day ".txt"))]
    (str/split-lines content)))

(defn -main []
  (println "Day 1")
  (let [start (System/nanoTime)]
    (day1/-main (readFile "01"))
    (let [end (System/nanoTime)]
      (println "Took " (/ (- end start) 1e6) "ms")))
  (println "Day 2")
  (let [start (System/nanoTime)]
    (day2/-main (readFile "02"))
    (let [end (System/nanoTime)]
      (println "Took " (/ (- end start) 1e6) "ms"))))