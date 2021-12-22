(ns advent-of-code.year2021.day22
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.zip :as z])
  (:import
   [java.io StringReader PushbackReader]
   [java.util BitSet]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example (slurp "inputs/year2021/day22.example"))

(def example2 (slurp "inputs/year2021/day22.example2"))

(def input (slurp "inputs/year2021/day22"))

(defn parse-line [s]
  (let [[_ on? & coords] (re-matches #"(on|off) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)" s)
        [minx maxx miny maxy minz maxz] (map parse-long coords)
        on? (case on? "on" true "off" false)]
    {:on?  on?
     :minx (+ 50 minx)
     :maxx (+ 51 maxx)
     :miny (+ 50 miny)
     :maxy (+ 51 maxy)
     :minz (+ 50 minz)
     :maxz (+ 51 maxz)}))

(defn parse [input]
  (->> (str/split-lines input)
    (mapv parse-line)))

(defn limit [x]
  (max 0 (min x 101)))

(defn part1
  ([] (part1 input))
  ([input]
   (let [cubes (parse input)
         bits  (BitSet. 1030301)]
     (doseq [{:keys [on? minx maxx miny maxy minz maxz]} cubes
             z (range (limit minz) (limit maxz))
             y (range (limit miny) (limit maxy))
             :let [offset (+ (* z 10201) (* y 101))]]
       (.set bits (+ offset (limit minx)) (+ offset (limit maxx)) on?))
     (.cardinality bits))))

(defn pairs [& xs]
  (let [xs (sort (set xs))]
    (map vector xs (next xs))))

(defn inside? [c1 c2]
  (and
    (<= (:minx c2) (:minx c1) (:maxx c1) (:maxx c2))
    (<= (:miny c2) (:miny c1) (:maxy c1) (:maxy c2))
    (<= (:minz c2) (:minz c1) (:maxz c1) (:maxz c2))))

(defn subtract [c1 c2]
  (if (or
        (<= (:maxx c2) (:minx c1))
        (<= (:maxy c2) (:miny c1))
        (<= (:maxz c2) (:minz c1))
        (<= (:maxx c1) (:minx c2))
        (<= (:maxy c1) (:miny c2))
        (<= (:maxz c1) (:minz c2)))
    [c1]
    (for [[x1 x2] (pairs (:minx c1) (:maxx c1) (:minx c2) (:maxx c2))
          [y1 y2] (pairs (:miny c1) (:maxy c1) (:miny c2) (:maxy c2))
          [z1 z2] (pairs (:minz c1) (:maxz c1) (:minz c2) (:maxz c2))
          :let [cube {:minx x1 :maxx x2 :miny y1 :maxy y2 :minz z1 :maxz z2}]
          :when (inside? cube c1)
          :when (not (inside? cube c2))]
      cube)))

(defn volume [c]
  (*
    (- (:maxx c) (:minx c))
    (- (:maxy c) (:miny c))
    (- (:maxz c) (:minz c))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [cubes  (parse input)
         cubes' (reduce
                  (fn [acc cube]
                    (let [acc' (mapcat #(subtract % cube) acc)]
                      (if (:on? cube)
                        (conj acc' cube)
                        acc')))
                  []
                  cubes)]
      (transduce (map volume) + 0 cubes'))))


(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day22 :reload))
  (part1 example)
  (part1 input)
  (part2 example2)
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))
