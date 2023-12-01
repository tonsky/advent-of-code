(ns advent-of-code.year2023.dayXX
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [advent-of-code.core :as core]))

(def sample1
  "")

(def data
  (slurp (io/file "inputs/year2023/dayXX")))

(defn parse [data]
  data)

(comment
  (parse sample1)
  (parse data))

(defn part1 [data]
  (let [data (parse data)]
    ))

(comment
  (part1 sample1)
  (part1 data))

(defn parse2 [data]
  (parse data))

(defn part2 [data]
  (let [data (parse2 data)]
    ))

(comment
  (part2 sample1)
  (part2 data))
