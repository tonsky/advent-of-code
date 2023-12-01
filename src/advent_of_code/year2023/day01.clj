(ns advent-of-code.year2023.day01
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [advent-of-code.core :as core]))

(def sample1
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def data
  (slurp (io/file "inputs/year2023/day01")))

(defn parse [data]
  (->> data
    (str/split-lines)))

(comment
  (parse sample1)
  (parse data))

(defn process1 [s]
  (let [digits (re-seq #"\d" s)
        a      (first digits)
        b      (last digits)]
    (parse-long (str a b))))

(defn part1 [data]
  (let [data (parse data)]
    (reduce + 0 (map process1 data))))

(comment
  (map process (parse sample1))
  (part1 sample1)
  (part1 sample2)
  (part1 data))

(def sample2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def m
  {"0" 0
   "1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9
   "one" 1
   "two" 2
   "three" 3
   "four"  4
   "five"  5
   "six"   6
   "seven" 7
   "eight" 8
   "nine"  9
   "zero"  0})

(defn find-last [re s]
  (loop [i (count s)]
    (if-some [m (re-find re (subs s i))]
      m
      (recur (dec i)))))

(defn process2 [s]
  (let [re     #"\d|one|two|three|four|five|six|seven|eight|nine"
        a      (m (re-find re s))
        b      (m (find-last re s))]
    (parse-long (str a b))))

(defn part2 [data]
  (let [data (parse data)]
    (reduce + 0 (map process2 data))))

(comment
  (part2 sample2)
  (part2 data))
