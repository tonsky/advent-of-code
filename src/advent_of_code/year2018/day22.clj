(ns advent-of-code.year2022.day22
  (:require
    [advent-of-code.v2 :as v2 :refer [v2]]
    [advent-of-code.priority-queue :as priority-queue]
    [advent-of-code.map :as map]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.data.priority-map :as priority-map]
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

(def sample
  {:depth  510
   :target (v2 10 10)})

(def data
  {:depth  7863
   :target (v2 14 760)})

(defn make-terrain [{:keys [depth target]} size]
  (let [{w :x, h :y} size
        res (map/make size)]
    (doseq [y (range 0 h)
            x (range 0 w)
            :let [coord (v2 x y)
                  idx   (cond
                          (= 0 x y)        0
                          (= coord target) 0
                          (= 0 x)          (* y 48271)
                          (= 0 y)          (* x 16807)
                          :else            (* (map/get res (v2 (dec x) y))
                                             (map/get res (v2 x (dec y)))))
                  erosion (mod (+ idx depth) 20183)]]
      (map/set! res coord erosion))
    (doseq [y (range 0 h)
            x (range 0 w)
            :let [coord (v2 x y)]]
      (map/set! res coord (mod (map/get res coord) 3)))
    (assoc res
      :depth  depth
      :target target)))

(defn part1 [{:keys [depth target] :as data}]
  (let [terrain (make-terrain data (v2/add target (v2 1 1)))]
    (reduce + 0 (:arr terrain))))

(comment
  (part1 sample)
  (part1 data))

(def terrain
  (make-terrain data (v2 40 900)))

(def scores
  {:torch (-> (map/make (v2 (:w terrain) (:h terrain)))
            (map/fill 10000)
            (map/set! (v2 0 0) 0))
   :gear  (-> (map/make (v2 (:w terrain) (:h terrain)))
            (map/fill 10000))
   :none  (-> (map/make (v2 (:w terrain) (:h terrain)))
            (map/fill 10000))})

(defn cost-fn [[pos equip]]
  (+
    (map/get (scores equip) pos)
    (v2/dist pos (:target data))
    (if (= equip :torch) 0 7)))

(def *queue
  (atom
    (priority-queue/priority-queue-by cost-fn
      [(v2 0 0) :torch])))

(defn allowed? [pos equip]
  (let [t (map/get terrain pos)]
    (or
      (and (= 0 t) (or (= :gear equip) (= :torch equip)))    ; rocky
      (and (= 1 t) (or (= :gear equip) (= :none equip)))     ; wet
      (and (= 2 t) (or (= :torch equip) (= :none equip)))))) ; narrow

(defn step [queue]
  (when-some [[pos equip] (priority-queue/peek queue)]
    (reduce
      (fn [queue [pos' equip' cost]]
        (cond+
          (nil? pos')
          queue
          
          (not (map/contains? terrain pos'))
          queue
                    
          (not (allowed? pos' equip'))
          queue
          
          :let [score  (map/get (scores equip') pos')
                score' (+ (map/get (scores equip) pos) cost)]
          
          (>= score' score)
          queue
          
          (>= score' (+
                       (* 1 (v2/dist pos' (:target terrain)))
                       (map/get (scores :torch) (:target terrain))))
          queue
          
          :else
          (do
            (map/set! (scores equip') pos' score')
            (priority-queue/conj queue [pos' equip']))))
      (priority-queue/pop queue)
      [[(v2/left pos)   equip 1]
       [(v2/right pos)  equip 1]
       [(v2/up pos)     equip 1]
       [(v2/down pos)   equip 1]
       (when (not= :torch equip)
         [pos :torch 7])
       (when (not= :gear equip)
         [pos :gear 7])
       (when (not= :none equip)
         [pos :none 7])])))

(comment
  @*queue
  (swap! *queue step))

(def fill-rocky
  (paint/fill 0xFFfaedcd))

(def fill-wet
  (paint/fill 0xFFe9edc9))

(def fill-narrow
  (paint/fill 0xFFfefae0))

(def fill-visited
  [(paint/fill 0x20000000)
   (paint/fill 0x30000000)
   (paint/fill 0x40000000)])

(def fill-queue
  (paint/fill 0x80000000))

(def fill-target
  (paint/fill 0xFFFF0000))

(defn on-paint [ctx canvas size]
  (let [{:keys [width height]} size
        scale 11
        cols  6
        col-h (quot (:h terrain) cols)
        conv  (fn [{:keys [x y]}]
                (v2
                  (+ x 2 (* (quot y col-h) (+ (:w terrain) 2)))
                  (+ 2 (mod y col-h))))]
    (doseq [y (range 0 (:h terrain))
            x (range 0 (:w terrain))
            :let [pos (v2 x y)
                  {cx :x cy :y} (conv pos)
                  rect (core/rect-xywh (* scale cx) (* scale cy) scale scale)]]
      (canvas/draw-rect canvas rect
        (case (map/get terrain pos)
          0 fill-rocky
          1 fill-wet
          2 fill-narrow))
      
      (when (= pos (:target data))
        (canvas/draw-rect
          canvas
          (core/rect-xywh (* scale cx) (* scale cy) scale scale)
          fill-target))

      (let [visited (+
                      (if (< (map/get (:torch scores) pos) 10000) 1 0)
                      (if (< (map/get (:gear scores) pos) 10000) 1 0)
                      (if (< (map/get (:none scores) pos) 10000) 1 0))]
        (when (> visited 0)
          (canvas/draw-rect canvas rect (nth fill-visited visited)))))

    (doseq [[pos _] (priority-queue/seq @*queue)
            :let [{cx :x cy :y} (conv pos)]]
      (canvas/draw-rect
        canvas
        (core/rect-xywh (* scale cx) (* scale cy) scale scale)
        fill-queue))

    (canvas/draw-string canvas 
      (str "Score: " (map/get (:torch scores) (:target data)))
      (- width 200) (- height 40) (:font-ui ctx) (:fill-text ctx))

    #_(gui/redraw)
    
    (when (not (empty? @*queue))
      (dotimes [i 100]
        (swap! *queue step))
      (gui/redraw))))

(reset! gui/*app
  (ui/default-theme
    (ui/canvas
      {:on-paint on-paint})))

(gui/redraw)

(comment
  (reset! *queue
    (priority-queue/priority-queue-by cost-fn
      [(v2 0 0) :torch])) 
  (while (not (empty? @*queue))
    (swap! *queue step))
  (swap! *queue step)
  (count @*queue)
  (first @*queue)
  (doseq [y (range 0 21)]
    (println)
    (doseq [x (range 0 21)]
      (print (map/get (:torch scores) (v2 x y)) ""))))
          