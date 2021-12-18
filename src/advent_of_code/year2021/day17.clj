(ns advent-of-code.year2021.day17
  (:refer-clojure :exclude [eval])
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example {:left 20 :right 30 :top -5 :bottom -10})

(def input {:left 153 :right 199 :top -75 :bottom -114})

(defn dec-abs ^long [^long x]
  (cond
    (pos? x)  (dec x)
    (neg? x)  (inc x)
    (zero? x) 0))

(defn shoot [{:keys [left right top bottom]} vx vy]
  (loop [x 0
         y 0
         vx (long vx)
         vy (long vy)
         maxy 0]
    (cond
      (and (< x left) (= 0 vx)) :overshoot
      (> vy (- bottom))         :overshoot
      (> x right)               :overshoot
      (< y bottom)              :undershoot
      (and (<= left x right) (<= bottom y top)) maxy
      :else (recur (+ x vx) (+ y vy) (dec-abs vx) (dec vy) (max maxy y)))))

(defn options [{:keys [left right top bottom] :as input}]
  (loop [vx   (-> (* 2 left) (Math/sqrt) (long))
         vy   bottom
         acc  []]
    (let [shot (shoot input vx vy)]
      (cond
        (> vx right)         acc
        (= :undershoot shot) (recur vx       (inc vy) acc)
        (number? shot)       (recur vx       (inc vy) (conj acc [vx vy shot]))
        (= :overshoot shot)  (recur (inc vx) bottom   acc)))))

(defn part1
  ([] (part1 input))
  ([input]
   (->> (options input)
     (map #(nth % 2))
     (reduce max))))

(defn part2
  ([] (part2 input))
  ([input]
   (->> (options input)
     (map #(take 2 %))
     (into #{})
     (count))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day17 :reload))
  (shoot example 7 2)
  (shoot example 6 3)
  (shoot example 9 0)
  (shoot example 17 -4)
  (shoot example 6 9)

  (part1 example)
  (part1 input)

  (part2 example)
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))