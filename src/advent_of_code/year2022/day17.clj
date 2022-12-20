(ns advent-of-code.year2022.day17
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
  :require [zip between? now])

(def sample1
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def data
  (slurp (io/file "inputs/year2022/day17")))

(defn parse [data]
  (mapv #(case % \> 1 \< -1) data))

(def shapes
  [(fn [y]
     [(v2 2 y) (v2 3 y) (v2 4 y) (v2 5 y)])
   (fn [y]
     [(v2 3 y) (v2 2 (dec y)) (v2 3 (dec y)) (v2 4 (dec y)) (v2 3 (- y 2))])
   (fn [y]
     [(v2 2 y) (v2 3 y) (v2 4 y) (v2 4 (dec y)) (v2 4 (- y 2))])
   (fn [y]
     [(v2 2 y) (v2 2 (dec y)) (v2 2 (- y 2)) (v2 2 (- y 3))])
   (fn [y]
     [(v2 2 y) (v2 3 y) (v2 2 (dec y)) (v2 3 (dec y))])])

(def reserve
  10000)

(def offset
  (quot reserve 2))

(def high-waterpoint
  10)

(def *state
  (atom
    {:grid       (grid/make (v2 7 reserve))
     :next-shape 0
     :blocks     nil
     :input      (parse data)
     :wind       0
     :high       0
     :top        (- reserve)
     :rocks      0}))

(defn grid-get [{:keys [grid top]} [x y]]
  (grid/get grid (v2 x (- y top))))

(defn grid-set! [{:keys [grid top]} [x y] val]
  (grid/set! grid (v2 x (- y top)) val))

(defn maybe-create [{:keys [grid next-shape blocks wind high] :as state}]
  (if blocks
    state
    (let [y      (- high 4)
          blocks ((nth shapes next-shape) y)]
      (assoc state
        :next-shape (mod (inc next-shape) 5)
        :blocks     blocks))))

(defn maybe-push [{:keys [grid next-shape blocks input wind high top] :as state}]
  (let [dir     (nth input wind)
        wind'   (mod (inc wind) (count input))
        blocks' (mapv (fn [[x y]] (v2 (+ x dir) y)) blocks)
        valid?  (every? #(and
                           (between? (:x %) 0 7)
                           (nil? (grid-get state %)))
                  blocks')]
    (cond-> state
      true   (assoc :wind wind')
      valid? (assoc :blocks blocks'))))

(defn maybe-fall [{:keys [grid next-shape blocks input wind high rocks] :as state}]
  (let [blocks' (mapv (fn [[x y]] (v2 x (inc y))) blocks)
        valid?  (every? #(and
                           (< (:y %) 0)
                           (nil? (grid-get state %))) blocks')]
    (if valid?
      (assoc state :blocks blocks')
      (do
        (doseq [block blocks]
          (grid-set! state block next-shape))
        (assoc state
          :blocks nil
          :rocks  (inc rocks)
          :high   (reduce min high (map :y blocks)))))))

(defn maybe-shift [{:keys [grid high top] :as state}]
  (if (< (- high top) high-waterpoint)
    (do
      (doseq [y (range (dec (:h grid)) (dec offset) -1)
              x (range 0 7)]
        (grid/set! grid (v2 x y) (grid/get grid (v2 x (- y offset)))))
      (doseq [y (range (dec offset) -1 -1)
              x (range 0 7)]
        (grid/set! grid (v2 x y) nil))
      (assoc state
        :top (- top offset)))
    state))

(defn step [state]
  (-> state
    (maybe-create)
    (maybe-push)
    (maybe-fall)
    (maybe-shift)))

(def fill-glass
  (paint/fill 0xFFfefae0))

(def fill-block
  (paint/fill 0xFF457b9d))

(def fill-rocks
  [(paint/fill 0xFFef476f)
   (paint/fill 0xFFffd166)
   (paint/fill 0xFF06d6a0)
   (paint/fill 0xFF118ab2)
   (paint/fill 0xFF9d4edd)])

(defn on-paint [ctx canvas size]
  (let [{:keys [grid blocks top next-shape] :as state} @*state]
    (let [scale 4]
      (canvas/with-canvas canvas
        (canvas/scale canvas scale scale)
        (canvas/draw-rect canvas (core/rect-xywh 0 0 (:w grid) (:h grid)) fill-glass)
      
        (doseq [y (range 0 (:h grid))
                x (range 0 (:w grid))
                :let [rock (grid/get grid (v2 x y))]
                :when rock]
          (canvas/draw-rect canvas (core/rect-xywh x y 1 1) (nth fill-rocks rock)))
      
        (when blocks
          (doseq [[x y] blocks
                  :let [y' (- y top)]]
            (canvas/draw-rect canvas (core/rect-xywh x y' 1 1) (nth fill-rocks next-shape))))))
    
    (let [scale 18]
      (canvas/with-canvas canvas
        (canvas/translate canvas 100 0)
        (canvas/scale canvas scale scale)
        (doseq [y (range -100 -200 -1)
                x (range 0 7)
                :let [rock (grid-get state (v2 x y))]
                :when rock]
          (canvas/draw-rect canvas (core/rect-xywh x (+ 200 y) 1 1) (nth fill-rocks rock)))))))
        
      
    

(comment
  (while (< (:rocks @*state) 2022)
    (swap! *state step)
    (gui/redraw)
    (Thread/sleep 2))
  (do
    (swap! *state step)
    (gui/redraw))
  (- 10000 (:high @*state)))


(reset! gui/*app
  (ui/default-theme
    (ui/canvas
      {:on-paint on-paint})))

(gui/redraw)

(defn part1
  ([data]
   (part1 data 2022))
  ([data steps]
   (let [data (parse data)
         t0   (now)]
     (loop [state {:grid       (grid/make (v2 7 reserve))
                   :next-shape 0
                   :blocks     nil
                   :input      data
                   :wind       0
                   :high       0
                   :top        (- reserve)
                   :rocks      0}
            last-t (now)]
       (let [now  (now)
             upd? (when (> now (+ last-t 1000))
                    (let [rocks (:rocks state)
                          perc  (-> rocks (/ steps) (* 100) int)
                          speed (-> (:rocks state) (/ (- now t0)) (* 1000) (int))
                          eta   (int (/ (- steps (:rocks state)) speed))]
                      (println (format "rocks %d, percent %d%%, speed %d/sec, eta %d sec" rocks perc speed eta))) 
                    true)]
         (if (>= (:rocks state) steps)
           (- (:high state))
           (recur (step state) (if upd? now last-t))))))))

(comment
  (part1 sample1)
  (part1 data))

(defn part2 [data]
  (let [data (parse data)
        state (loop [state {:grid       (grid/make (v2 7 reserve))
                            :next-shape 0
                            :blocks     nil
                            :input      data
                            :wind       0
                            :high       0
                            :top        (- reserve)
                            :rocks      0}]
                (if (<= (:high state) (- 10 (:h (:grid state))))
                  state
                  (recur (step state))))
        _     (reset! *state state)
        _     (gui/redraw)
        grid  (:grid state)
        fp-fn (fn [y]
                (vec
                  (for [y (range y (- y 25) -1)
                        x (range 0 7)]
                    (grid-get state (v2 x y)))))
        fp   (fp-fn -1000)]
    ; (println fp)
    (some
      #(do
         ; (println % (fp-fn %))
         (when (= (fp-fn %) fp)
           %))
      (range -1001 (+ (- (:h grid)) 5) -1))))

(defn part2-sample []
  (let [data   (parse sample1)
        state1 (loop [state {:grid       (grid/make (v2 7 1000))
                             :next-shape 0
                             :blocks     nil
                             :input      data
                             :wind       0
                             :high       0
                             :top        (- 1000)
                             :rocks      0}]
                 (if (< (:high state) -100)
                   state
                   (recur (step state))))
        rocks'  (- 1000000000000 (:rocks state1))
        repeats (quot rocks' 35)
        height  (* repeats 53)
        state2 (loop [state (-> state1
                              (update :high - height)
                              (update :top - height)
                              (update :rocks + (* 35 repeats)))]
                 (if (>= (:rocks state) 1000000000000)
                   state
                   (recur (step state))))]
    (- (:high state2))))

(defn part2-data []
  (let [data (parse data)
        state1 (loop [state {:grid       (grid/make (v2 7 10000))
                             :next-shape 0
                             :blocks     nil
                             :input      data
                             :wind       0
                             :high       0
                             :top        (- 10000)
                             :rocks      0}]
                 (if (< (:high state) -1000)
                   state
                   (recur (step state))))
        state2 (loop [state state1]
                 (if (< (:high state) -3702)
                   state
                   (recur (step state))))
        dr      (- (:rocks state2) (:rocks state1))
        dh      2702
        rocks'  (- 1000000000000 (:rocks state2))
        repeats (quot rocks' dr)
        height  (* repeats dh)
        state3  (loop [state (-> state2
                               (update :high - height)
                               (update :top - height)
                               (update :rocks + (* dr repeats)))]
                  (if (>= (:rocks state) 1000000000000)
                    state
                    (recur (step state))))]
    (- (:high state3))))
                   

(comment
  (part2 sample1) ;; 1000 -> 1053 ;; 35 figures, +53 height
  (part2-sample)  ;; 1514285714288
  
  (part2 data)    ;; 1000 -> 3702
  (part2-data)
  
  (gui/redraw))
