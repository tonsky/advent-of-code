(ns advent-of-code.year2022.day05
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
    [io.github.humbleui.window :as window])
  (:import
    [io.github.humbleui.skija Color Paint]))

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(def sample
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def data
  (slurp (io/file "inputs/year2022/day05")))

(defn transpose [xss]
  (apply mapv vector xss))

(defn parse-stacks [data]
  (let [; filter input for stack lines only
        lines (->> data
                (str/split-lines)
                (filter #(re-find #"\[" %)))
        ; look for the sequence of either [\w] or \s\s\s in each line
        rows  (map #(map second (re-seq #"(?:\[(\w)\]| ( ) ) ?" %)) lines)
        ; transpose, reverse and drop nils
        cols  (->> (transpose rows)
                (mapv #(->> % reverse (take-while some?) vec)))]
    cols))

(comment
  (parse-stacks sample)
  (parse-stacks data))

(defn parse-moves [data]
  (let [; filter input for moves lines only
        lines (->> data
                (str/split-lines)
                (filter #(str/starts-with? % "move ")))]
    (vec
      (for [line lines
            :let [[_ amount from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]]
        {:from   (dec (parse-long from)) ; zero-based index
         :to     (dec (parse-long to))   ; zero-based index
         :amount (parse-long amount)}))))

(comment
  (parse-moves sample)
  (parse-moves data))

(defn move-one [stacks from to]
  (let [el (peek (nth stacks from))] ; get last
    (-> stacks
      (update from pop)              ; remove last
      (update to conj el))))         ; add to the end

(defn move [stacks {:keys [from to amount]}]
  (reduce
    (fn [s _]
      (move-one s from to))
    stacks
    (range amount))) ; repeat `amount` times

(defn part1 [data]
  (let [stacks  (parse-stacks data)
        moves   (parse-moves data)
        stacks' (reduce move stacks moves)]
    (str/join (map peek stacks'))))

(comment
  (part1 sample)
  (part1 data))

(defn move2 [stacks {:keys [from to amount]}]
  (let [stack (take-last amount (nth stacks from))]
    (-> stacks
      (update from #(vec (drop-last amount %)))
      (update to   #(vec (concat % stack))))))

(defn part2 [data]
  (let [stacks  (parse-stacks data)
        moves   (parse-moves data)
        stacks' (reduce move2 stacks moves)]
    (str/join (map peek stacks'))))

(comment
  (part2 sample)
  (part2 data))

(def *stacks
  (atom (parse-stacks data)))

(def *moves
  (atom nil))

(def ^Paint paint-bg
  (paint/fill 0xFF000000))

(def ^Paint paint-text
  (paint/fill 0xFFFFFFFF))

(defn hue->rgb [p q t]
  (let [t (if (< t 0) (+ t 1) t)
        t (if (> t 1) (- t 1) t)]
    (cond
      (< t 1/6) (+ p (* (- q p) 6 t))
      (< t 1/2) q
      (< t 2/3) (+ p (* (- q p) (- 2/3 t) 6))
      :else     p)))

(defn hsl->rgb [h s l]
  (let [q (if (< l 0.5) (* l (+ 1 s)) (+ l s (- (* l s))))
        p (- (* 2 l) q)
        r (hue->rgb p q (+ h 1/3))
        g (hue->rgb p q h)
        b (hue->rgb p q (- h 1/3))]
    [r g b]))

(defn color [s] ;; "A".."Z"
  (let [a (.codePointAt "A" 0)
        z (.codePointAt "Z" 0)
        n (-> (.codePointAt s 0) (- a) (/ (+ (- z a) 1)) double)
        [r g b] (hsl->rgb n 0.75 0.4)]
    (Color/makeRGB (* r 255) (* g 255) (* b 255))))

(defn on-paint [ctx canvas size]
  (let [{:keys [font-ui fill-text scale]} ctx
        {:keys [width height]} size
        dim (min (quot width 10) (quot height 20))
        left (-> width (- (* dim 9)) (quot 2))]
    (doseq [[column x] (zip @*stacks (range))
            [row y]    (zip column (range))
            :let [rx   (+ left (* x dim))
                  ry   (- height dim (* y dim))
                  rect (core/rect-xywh rx ry (dec dim) (dec dim))]]
      (.setColor paint-bg (color row))
      (canvas/draw-rect canvas rect paint-bg)
      (canvas/draw-string canvas row (+ rx (* 8 scale)) (+ ry (* 19 scale)) font-ui paint-text)))
  (when-some [m (first @*moves)]
    (swap! *stacks move2 m)
    (swap! *moves #(vec (next %)))
    (gui/redraw)))

(reset! gui/*app
  (ui/default-theme
    (ui/canvas
      {:on-paint on-paint})))

(gui/redraw)

(comment
  (window/set-z-order @gui/*window :floating))
  (do
    (reset! *stacks (parse-stacks data))
    (reset! *moves (parse-moves data))
    (gui/redraw))
