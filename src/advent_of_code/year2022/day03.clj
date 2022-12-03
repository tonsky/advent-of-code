(ns advent-of-code.year2022.day03
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(def alphabet
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def priorities
  (into {} (zip alphabet (range 1 53))))

(defn parse-rucksack [s]
  (let [len (count s)]
    [(set (take (/ len 2) s))
     (set (drop (/ len 2) s))]))

(defn parse [data]
  (->> (str/split-lines data)
    (mapv parse-rucksack)))

(def sample1
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def data
  (slurp (io/file "inputs/year2022/day03")))

(defn part1 [data]
  (reduce + 0
    (for [[l r] (parse data)
          :let [in (set/intersection l r)]]
      (priorities (first in)))))    

(comment
  (part1 sample1)
  (part1 data))

(defn parse2 [data]
  (->> (str/split-lines data)
    (map set)
    (partition 3)))

(defn part2 [data]
  (reduce + 0
    (for [[a b c] (parse2 data)
          letter alphabet
          :let [num (+
                      (if (a letter) 1 0)
                      (if (b letter) 1 0)
                      (if (c letter) 1 0))]
          :when (= num 3)]
      (priorities letter))))

(comment
  (part2 sample1)
  (part2 data))
