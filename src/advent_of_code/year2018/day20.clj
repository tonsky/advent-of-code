(ns advent-of-code.year2018.day20
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [advent-of-code.year2022.gui :as gui]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.ui :as ui]
    #_[remote-require.core :as rr])
  (:import
    [io.github.humbleui.skija Color Paint]))
 
(def sample1
  "WNE") ; => 3

(def sample2
  "ENWWW(NEEE|SSE(EE|N))") ; => 10

(def sample3
  "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN") ; => 18

(def sample4
  "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))") ; => 23

(def sample5
  "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))") ; => 31

(def data
  (slurp (io/file "inputs/year2018/day20")))

(defn parse [data]
  (-> (str "[" data "]")
    (str/lower-case)
    (str/replace "(" "[[")
    (str/replace ")" "]]")
    (str/replace "|" "][")
    (str/replace #"\w" ":$0 ")
    (clojure.edn/read-string)))

(def *map
  (atom {[0 0] 0}))

(add-watch *map ::redraw
  (fn [_ _ _ _]
    (gui/redraw)))

(defn walk-one [[x y] [dx dy] step]
  (let [[x' y'] [(+ x dx) (+ y dy)]
        v (get @*map [x' y'] Long/MAX_VALUE)]
    (when (< (inc step) v)
      (swap! *map #(merge-with min % {[x' y'] (inc step)}))
      [x' y'])))

(defn walk [[x y] step path]
  (let [dir (first path)]
    (case dir
      :n (when-some [[x' y'] (walk-one [x y] [0 -1] step)]
           (recur [x' y'] (inc step) (next path)))
      :s (when-some [[x' y'] (walk-one [x y] [0 1] step)]
           (recur [x' y'] (inc step) (next path)))
      :w (when-some [[x' y'] (walk-one [x y] [-1 0] step)]
           (recur [x' y'] (inc step) (next path)))
      :e (when-some [[x' y'] (walk-one [x y] [1 0] step)]
           (recur [x' y'] (inc step) (next path)))
      (doseq [branch dir]
        (walk [x y] step (concat branch (next path)))))))
      
(defn part1 [data]
  (reset! *map {[0 0] 0})
  (walk [0 0] 0 (parse data))
  (reduce max (vals @*map)))

(comment
  (part1 sample1)
  (part1 sample2)
  (part1 sample3)
  (part1 sample4)
  (part1 sample5)
  (part1 data))

(defn part2 [data]
  (reset! *map {[0 0] 0})
  (walk [0 0] 0 (parse data))
  (->> (vals @*map)
    (filter #(>= % 1000))
    (count)))

(comment
  (part2 sample5)
  (part2 data))

(def ^Paint paint-bg
  (paint/fill 0x20CC3333))

(defn on-paint [ctx canvas size]
  (let [{:keys [font-ui fill-text scale]} ctx
        {:keys [width height]} size
        m @*map
        min-x (->> (keys m) (map first) (reduce min))
        max-x (->> (keys m) (map first) (reduce max))
        min-y (->> (keys m) (map second) (reduce min))
        max-y (->> (keys m) (map second) (reduce max))
        size  (min
                (/ width (inc (- max-x min-x)))
                (/ height (inc (- max-y min-y))))
        min-v (->> (vals m) (reduce min))
        max-v (->> (vals m) (reduce max))]
    (doseq [[[x y] v] m
            :let [rect (core/rect-xywh
                         (* (- x min-x) size)
                         (* (- y min-y) size)
                         (- size scale)
                         (- size scale))
                  rate (/ (- v min-v) (max 1 (- max-v min-v)))
                  color (Color/makeLerp (unchecked-int 0xFFFF0000) (unchecked-int 0xFF00FF00) rate)]]
      (.setColor paint-bg color)
      (canvas/draw-rect canvas rect paint-bg))))

(reset! gui/*app
  (ui/default-theme
    (ui/canvas
      {:on-paint on-paint})))

(gui/redraw)

(comment
  (rr/from "https://github.com/weavejester/medley/master/src/medley/core.cljc"
    :require [assoc-some]))

