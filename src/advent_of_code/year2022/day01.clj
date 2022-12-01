(ns advent-of-code.year2022.day01
  (:require
    [advent-of-code.core :as core]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    #_[advent-of-code.year2022.gui :as gui]
    #_[io.github.humbleui.ui :as ui]
    #_[remote-require.core :as rr]))

(def sample
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(def data
  (slurp (io/file "inputs/year2022/day01")))

(defn parse [data]
  (->>
    (str/split data #"\n\n")
    (mapv #(->> (str/split % #"\n") (mapv parse-long)))))

(parse sample)

(defn part1 [data]
  )

(comment
  (core/measure (part1 sample))
  (core/measure (part1 data)))

(defn part2 [data]
  )

(comment
  (core/measure (part2 sample))
  (core/measure (part2 data)))

(comment
  (rr/from "https://github.com/weavejester/medley/master/src/medley/core.cljc"
    :require [assoc-some])

  (reset! gui/*app
    (ui/default-theme
      (ui/focus-controller
        (ui/center
          (ui/label "Day 01")))))

  (gui/redraw))
