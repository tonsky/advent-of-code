(ns advent-of-code.year2022.day18
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
  :require [zip])

(rr/from "https://twitter.com/nikitonsky/status/1602720736875057152"
  :require [index-of])

(def sample1
  "1,1,1
2,1,1")

(def sample2
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(def data
  (slurp (io/file "inputs/year2022/day18")))

(defn parse [data]
  (->> data
    (str/split-lines)
    (mapv #(->> (str/split % #",") (mapv parse-long)))
    (set)))

(comment
  (parse sample1)
  (parse data))

(defn third [xs]
  (nth xs 2))

(defn part1 [data]
  (let [data (parse data)]
    (count
      (for [[x y z] data
            n [[(+ x 1) y z]
               [(- x 1) y z]
               [x (+ y 1) z]
               [x (- y 1) z]
               [x y (+ z 1)]
               [x y (- z 1)]]
            :when (not (data n))]
        1))))

(comment
  (part1 sample1)
  (part1 sample2)
  (part1 data))

(defn outside [data min max]
  (loop [queue [[min min min]]
         outside #{}]
    (cond+
      (empty? queue) outside
      :let [[x y z :as v] (peek queue)]
      (data v)    (recur (pop queue) outside)
      (outside v) (recur (pop queue) outside)
      :else       (recur (cond-> (pop queue)
                           (> x min)       (conj [(dec x) y z])
                           (< x (dec max)) (conj [(inc x) y z])
                           (> y min)       (conj [x (dec y) z])
                           (< y (dec max)) (conj [x (inc y) z])
                           (> z min)       (conj [x y (dec z)])
                           (< z (dec max)) (conj [x y (inc z)]))
                    (conj outside v)))))


(defn part2 [orig]
  (let [data    (parse orig)
        min     -1
        max     25
        outside (outside data min max)
        sides   (for [[x y z :as v] outside
                      [x' y' z' :as v'] [[(+ x 1) y z]
                                         [(- x 1) y z]
                                         [x (+ y 1) z]
                                         [x (- y 1) z]
                                         [x y (+ z 1)]
                                         [x y (- z 1)]]
                      :when (<= min x' (dec max))
                      :when (<= min y' (dec max))
                      :when (<= min z' (dec max))
                      :when (not (outside v'))]
                  1)]
    (count sides)))

(comment
  (part2 sample1)
  (part2 sample2)
  (part2 data))

(def fill-data
  (paint/fill 0xFFe63946))

(def fill-outside
  (paint/fill 0xFFfcbf49))

(defn on-paint [ctx canvas _]
  (let [data     (parse data)
        min      -1
        max      25
        outside  (set (outside data min max))]
    (doseq [z (range min max)
            y (range min max)
            x (range min max)
            :let [v [x y z]
                  x (- x min)
                  y (- y min)
                  z (- z min)]]
      (canvas/with-canvas canvas
        (canvas/scale canvas 15)
        (canvas/translate canvas (* 26 (mod z 7)) (* 26 (quot z 7)))
        
        (when (data v)
          (canvas/draw-rect canvas (core/rect-xywh x y 1 1) fill-data))
                
        (when (outside v)
          (canvas/draw-rect canvas (core/rect-xywh x y 1 1) fill-outside))))))

(reset! gui/*app
  (ui/default-theme
    (ui/canvas
      {:on-paint on-paint})))

(gui/redraw)