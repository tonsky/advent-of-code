(ns advent-of-code.year2022.day13
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(rr/from "https://twitter.com/nikitonsky/status/1602720736875057152"
  :require [index-of])

(def sample1
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(def data
  (slurp (io/file "inputs/year2022/day13")))

(defn parse [data]
  (->> data
    (str/split-lines)
    (remove str/blank?)
    (mapv edn/read-string)))

(comment
  (parse sample1)
  (parse data))

(defn cmp [a b]
  (cond
    (and (number? a) (number? b))
    (- a b)
    
    (and (vector? a) (vector? b))
    (or
      (->> (map cmp a b) ;; compare items while in both
        (drop-while zero?)
        (first))
      (- (count a) (count b)))
    
    (and (vector? a) (number? b))
    (cmp a [b])
    
    (and (number? a) (vector? b))
    (cmp [a] b)))

(defn part1 [data]
  (let [pairs (->> data parse (partition 2))]
    (reduce + 0
      (for [[[a b] idx] (zip pairs (range 1 Long/MAX_VALUE))
            :when (neg? (cmp a b))]
        idx))))

(comment
  (part1 sample1)
  (part1 data))

(defn part2 [data]
  (let [packets (-> (parse data)
                  (conj [[2]])
                  (conj [[6]])
                  (->>
                    (sort-by identity cmp)
                    vec))
        i1 (inc (index-of packets [[2]]))
        i2 (inc (index-of packets [[6]]))]
    (* i1 i2)))

(comment
  (part2 sample1)
  (part2 data))
