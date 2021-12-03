(ns advent-of-code.year2021.day3
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(def example
"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def input (slurp "inputs/year2021/day3"))

(defn ones? [lines pos]
  (let [chars (map #(nth % pos) lines)
        {zeros \0 ones \1} (frequencies chars)]
    (>= ones zeros)))

(defn invert [digits]
  (mapv #(- 1 %) digits))

(defn parse-digits [digits]
  (reduce
    (fn [acc d]
      (+ (bit-shift-left acc 1) d))
    0
    digits))

(defn part1
  ([] (part1 input))
  ([input]
   (let [lines  (str/split-lines input)
         digits (for [pos (range (count (first lines)))]
                  (if (ones? lines pos) 1 0))
         γ      (parse-digits digits)
         ε      (parse-digits (invert digits))]
     (* γ ε))))

(defn rating [lines keep-ones keep-zeros]
  (loop [lines lines
         pos   0]
    (if (<= (count lines) 1)
      (Long/parseLong (first lines) 2)
      (let [keep (if (ones? lines pos) keep-ones keep-zeros)]
        (recur (filter #(= keep (nth % pos)) lines) (inc pos))))))

(defn part2
  ([] (part2 input))
  ([input]
    (let [lines (str/split-lines input)
          O₂    (rating lines \1 \0)
          CO₂   (rating lines \0 \1)]
     (* O₂ CO₂))))

(defn -main [& args]
  (println "Day 3")
  (println "├ part 1:" (part1))
  (println "└ part 2:" (part2)))

(comment
  (part1 example)
  (part1)
  (part2 example)
  (part2))