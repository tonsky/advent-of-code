(ns advent-of-code.year2022.day14
  (:require
    [advent-of-code.v2 :as v2 :refer [v2]]
    [advent-of-code.grid :as grid]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]
    [advent-of-code.year2022.gui :as gui]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.window :as window]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(def sample1
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def data
  (slurp (io/file "inputs/year2022/day14")))

(declare grid)

(def *sand
  (atom nil))

(defn parse-line [s]
  (let [seq (-> s
              (str/split #" -> ")
              (->> (map #(apply v2 (map parse-long (str/split % #","))))))]
    (zip seq (next seq))))

(defn parse [data w h]
  (let [data (->> data
              (str/split-lines)
              (mapcat parse-line)
              (vec))
        g    (grid/make (v2 w h))]
    (doseq [[[x1 y1] [x2 y2]] data
            :let [x1 (-> x1 (- 500) (+ (quot w 2)))
                  x2 (-> x2 (- 500) (+ (quot w 2)))]]
      (cond
        (== x1 x2)
        (doseq [y (range (min y1 y2) (inc (max y1 y2)))]
          (grid/set! g (v2 x1 y) :rock))
        (== y1 y2)
        (doseq [x (range (min x1 x2) (inc (max x1 x2)))]
          (grid/set! g (v2 x y1) :rock))))
    (def grid g)
    (reset! *sand (v2 (quot w 2) 0))))

(def fill-air
  (paint/fill 0xFF4DBDFF))

(def fill-rock
  (paint/fill 0xFF56667D))

(def fill-sand
  (paint/fill 0xFFE3C47A))

(def fill-falling
  (paint/fill 0xFFB7424A))

(def size
  3)

(defn spawn []
  (let [pos (v2 (quot (:w grid) 2) 0)]
    (when (nil? (grid/get grid pos))
      pos)))

(defn spawn2 []
  (rand-nth
    (for [x (range (:w grid))
          :let [pos (v2 x 0)]
          :when (nil? (grid/get grid pos))]
      pos)))

(defn step [sand]
  (when-some [[x y] sand]
    (cond
      (>= (+ y 1) (:h grid))
      nil
      
      (nil? (grid/get grid (v2 x (+ y 1))))
      (v2 x (+ y 1))
      
      (nil? (grid/get grid (v2 (- x 1) (+ y 1))))
      (v2 (- x 1) (+ y 1))
      
      (nil? (grid/get grid (v2 (+ x 1) (+ y 1))))
      (v2 (+ x 1) (+ y 1))
      
      :else
      (do
        (grid/set! grid (v2 x y) :sand)
        (spawn)))))

(defn on-paint [ctx canvas _]
  (let [{:keys [w h]} grid
        size (* size (:scale ctx))]
    (canvas/draw-rect
      canvas
      (core/rect-xywh 0 0 (* w size) (* h size))
      fill-air)
    
    (doseq [y (range h)
            x (range w)
            :let [pos (v2 x y)
                  e   (grid/get grid (v2 x y))]
            :when e]
      (canvas/draw-rect
        canvas
        (core/rect-xywh (* x size) (* y size) size size)
        (case e
          :rock fill-rock
          :sand fill-sand)))
    (gui/redraw)))

(reset! gui/*app
  (ui/default-theme
    (ui/center
      (ui/dynamic _ [{:keys [w h]} grid]
        (ui/width (* size (:w grid))
          (ui/height (* size (:h grid))
            (ui/canvas
              {:on-paint on-paint})))))))

(defn result []
  (count
    (for [y (range (:h grid))
          x (range (:w grid))
          :when (= :sand (grid/get grid (v2 x y)))]
      1)))

(gui/redraw)

(defn part1 []
  (parse data 100 180)
  ; (Thread/sleep 1000)
  (while (some? (swap! *sand step))
    (while (some-> (swap! *sand step) :y (or 0) (> 0))
      true)
    #_(Thread/sleep 10))
  (result))

(defn part2 []
  (parse data 500 180)
  (doseq [x (range 0 (:w grid))]
    (grid/set! grid (v2 x 178) :rock))
  ; (Thread/sleep 1000)
  (while (some? (swap! *sand step))
    (Thread/sleep 0))
  (result))

(comment
  (part1)
  (part2))
  
  
