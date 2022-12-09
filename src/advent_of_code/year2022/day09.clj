(ns advent-of-code.year2022.day09
  (:require
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
  :require [zip clamp])

(def sample1
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def data
  (slurp (io/file "inputs/year2022/day09")))

(defn parse [data]
  (->> data
    str/split-lines
    (mapcat
      #(let [[_ d n] (re-matches #"(\w) (\d+)" %)]
         (repeat (parse-long n)
           (case d
             "R" :right
             "L" :left
             "U" :up
             "D" :down))))
    vec))

(def start1
  {:rope [[0 0] [0 0]]
   :visited #{[0 0]}})

(defonce *state
  (atom start1))

(defonce *steps
  (atom nil))

(defn follow [[hx hy] [tx ty]]
  (let [[dtx dty] (if (and
                        (<= -1 (- hx tx) 1)
                        (<= -1 (- hy ty) 1))
                    [0 0]
                    [(clamp (- hx tx) -1 1)
                     (clamp (- hy ty) -1 1)])
        [tx' ty'] [(+ tx dtx) (+ ty dty)]]
    [tx' ty']))

(defn step [state dir]
  (let [{:keys [rope visited]} state
        [hx hy]   (first rope)
        [hx' hy'] (case dir
                    :right [(inc hx) hy]
                    :left  [(dec hx) hy]
                    :up    [hx (dec hy)]
                    :down  [hx (inc hy)])
        rope'     (reduce
                    (fn [rope knot]
                      (conj rope (follow (peek rope) knot)))
                    [[hx' hy']]
                    (next rope))]
    {:rope rope'
     :visited (conj visited (peek rope'))}))

(defn part1 [data]
  (let [steps (parse data)
        state' (reduce step start1 steps)]
    (count (:visited state'))))

(comment
  (part1 sample1)
  (part1 data))

(def start2
  {:rope    (repeat 10 [0 0])
   :visited #{[0 0]}})

(def sample2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defn part2 [data]
  (let [steps (parse data)
        state' (reduce step start2 steps)]
    (count (:visited state'))))

(comment
  (part2 sample1)
  (part2 sample2)
  (part2 data))

(def fill-head
  (paint/fill 0xFF33CC33))

(def fill-tail
  (paint/fill 0xFFCC3333))

(def fill-visited
  (paint/fill 0xFF808080))

(defn on-paint [ctx canvas size]
  (let [{:keys [width height]} size
        scale 4
        {:keys [rope visited]} @*state]
    
    (doseq [[x y] visited]
      (canvas/draw-rect
        canvas
        (core/rect-xywh
          (+ (quot width 2) (* scale x))
          (+ (quot height 2) (* scale y))
          (- scale 0)
          (- scale 0))
        fill-visited))
    
    (doseq [[x y] rope]
      (canvas/draw-rect
        canvas
        (core/rect-xywh
          (+ (quot width 2) (* scale x))
          (+ (quot height 2) (* scale y))
          scale
          scale)
        fill-head))

    (when-some [s (first @*steps)]
      (swap! *state step s)
      (swap! *steps #(next %))
      (gui/redraw))))

(reset! gui/*app
  (ui/default-theme
    (ui/canvas
      {:on-paint on-paint})))

(gui/redraw)

(comment
  (float (/ (count @*steps) (count (parse data))))
  (do
    (reset! *steps (parse data))
    (reset! *state start2)
    (gui/redraw))
  (when-some [s (first @*steps)]
    (swap! *state step s)
    (swap! *steps #(next %))
    (gui/redraw))
  (window/set-z-order @gui/*window :normal)
  )
