(ns advent-of-code.year2022.day02
  (:require
    #_[advent-of-code.core :as core]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    #_[advent-of-code.year2022.gui :as gui]
    #_[io.github.humbleui.ui :as ui]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(defn parse [data]
  (->>
    (re-seq #"\w" data)
    (partition 2)))

(def sample1
  (parse "A Y
B X
C Z"))

(defn score [[l r]]
  (+ ({:rock 1 :paper 2 :scissors 3} r)
    (cond
      (= l r) 3
      (= [:rock :paper] [l r]) 6
      (= [:paper :scissors] [l r]) 6
      (= [:scissors :rock] [l r]) 6
      :else 0)))

(def data
  (parse (slurp (io/file "inputs/year2022/day02"))))

(defn part1 [data]
  (->> data
    (map (fn [[l r]]
           [({"A" :rock "B" :paper "C" :scissors} l)
            ({"X" :rock "Y" :paper "Z" :scissors} r)]))
    (map score)
    (reduce + 0)))

(comment
  (part1 sample1)
  (part1 data))

(defn score2 [[l r]]
  (case r
    :win (score [l ({:rock :paper
                     :paper :scissors
                     :scissors :rock} l)])
    :draw (score [l l])
    :lose (score [l ({:rock :scissors
                      :paper :rock
                      :scissors :paper} l)])))

(defn part2 [data]
  (->> data
    (map (fn [[l r]]
           [({"A" :rock "B" :paper "C" :scissors} l)
            ({"X" :lose "Y" :draw "Z" :win} r)]))
    (map score2)
    (reduce + 0)))

(comment
  (part2 sample1)
  (part2 data))

(comment
  (rr/from "https://github.com/weavejester/medley/master/src/medley/core.cljc"
    :require [assoc-some])

  (reset! gui/*app
    (ui/default-theme
      (ui/focus-controller
        (ui/center
          (ui/label "Day 02")))))

  (gui/redraw))
