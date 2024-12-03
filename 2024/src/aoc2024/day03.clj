(ns aoc2024.day03)

(def mul-pattern #"mul\((\d+),(\d+)\)")
(def do-pattern #"do\(\)")
(def dont-pattern #"don't\(\)")

(defn part1 [data]
  (reduce + (map #(* (Integer/parseInt (nth % 1)) (Integer/parseInt (nth % 2))) (re-seq mul-pattern data))))

(defn getIdx [pattern data]
  (let [matcher (re-matcher pattern data)]
    (loop [idx []] (if (.find matcher) (recur (conj idx (.start matcher))) idx))))

(defn mulInstruction [data [idx n1 n2]]
  (let [do-idx (last (cons 0 (filter #(< % idx) (getIdx do-pattern data))))
        dont-idx (last (cons 0 (filter #(< % idx) (getIdx dont-pattern data))))]
    (if (>= do-idx dont-idx) (* n1 n2) 0)))

(defn part2 [data]
  (let [muls (re-matcher mul-pattern data)
        mul-idxs (loop [idx []]
                   (if (.find muls)
                     (recur
                      (conj idx (list (.start muls) (Integer/parseInt (.group muls 1)) (Integer/parseInt (.group muls 2)))))
                     idx))]
    (reduce + (map #(mulInstruction data %) mul-idxs))))

(defn -main [input]
  (let [result (part1 input)]
    (println "Part 1: " result))
  (let [result (part2 input)]
    (println "Part 2: " result)))