(ns advent-of-code.year2022.day12
  (:require
    [advent-of-code.v2 :as v2 :refer [v2]]
    [advent-of-code.priority-queue :as priority-queue]
    [advent-of-code.map :as map]
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

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(def sample1
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(def data
  (slurp (io/file "inputs/year2022/day12")))

(defn parse [data]
  (let [lines  (str/split-lines data)
        width  (count (first lines))
        height (count lines)
        map    (map/make (v2 width height))
        *start (volatile! nil)
        *end   (volatile! nil)]
    (doseq [[y line] (zip (range) lines)
            [x ch]   (zip (range) line)
            :let [ch (case ch
                       \S (do (vreset! *start (v2 x y)) \a)
                       \E (do (vreset! *end (v2 x y)) \z)
                       ch)
                  e  (- (int ch) (int \a))]]
      (map/set! map (v2 x y) e))
    (with-meta map
      {:start @*start
       :end   @*end})))

(comment
  (parse sample1)
  (meta (parse sample1))
  (parse data))

(let [data data]
  (def terrain
    (parse data))
  
  (def start
    (:start (meta terrain)))
  
  (def end
    (:end (meta terrain)))
  
  (def costs
    (-> (map/make (v2 (:w terrain) (:h terrain)))
      (map/fill 9999)
      (map/set! start 0)))
  
  (def path
    (map/make (v2 (:w terrain) (:h terrain))))

  (defn cost-fn [p]
    (+
      (map/get costs p)
      (v2/dist p end)))
  
  (def *queue
    (atom 
      (priority-queue/priority-queue-by cost-fn
        start)))
  
  (def *queue2
    (atom 
      (-> clojure.lang.PersistentQueue/EMPTY
        (conj end)))))

(defn step [queue]
  (when-some [pos (priority-queue/peek queue)]
    (reduce
      (fn [queue pos']
        (cond+
          (nil? pos')
          queue
          
          (not (map/contains? terrain pos'))
          queue
          
          :let [e  (map/get terrain pos)
                e' (map/get terrain pos')]
          
          (> e' (+ e 1))
          queue
          
          :let [cost  (map/get costs pos')
                cost' (+ (map/get costs pos) 1)]
          
          (>= cost' cost)
          queue
          
          :else
          (do
            (map/set! costs pos' cost')
            (map/set! path pos' pos)
            (priority-queue/conj queue pos'))))
      (priority-queue/pop queue)
      (v2/neighbours4 pos))))

(defn part1 []
  (map/set! costs start 0)
  (while (not-empty @*queue)
    (swap! *queue step))
  (map/get costs end))

(comment
  (part1))

(defn on-paint [ctx canvas size]
  (let [{:keys [width height]} size
        scale 40]
    (with-open [paint (paint/fill 0xFFFFFFFF)]
      (doseq [y (range (:h terrain))
              x (range (:w terrain))
              :let [e  (map/get terrain (v2 x y))
                    ea (-> e (/ 25) (* 255) int)
                    c  (map/get costs (v2 x y))
                    ca (-> c (/ 350) (* 255) int)]]
        (if (< c 9999)
          (.setColor paint (Color/makeRGB 0 ca 0))
          (.setColor paint (Color/makeRGB ea ea ea)))
        (canvas/draw-rect
          canvas
          (core/rect-xywh (* x scale) (* y scale) scale scale)
          paint))
            
      (.setColor paint (unchecked-int 0x80FF0000))
      (doseq [{:keys [x y]} (priority-queue/seq @*queue)]
        (canvas/draw-rect 
          canvas
          (core/rect-xywh (* x scale) (* y scale) scale scale)
          paint))
      
      (loop [p end]
        (let [{:keys [x y]} p
              c  (map/get costs p)
              ca (-> c (/ 350) (* 255) int)]
          (.setColor paint (Color/makeRGB ca 0 0))
          (canvas/draw-rect canvas (core/rect-xywh (* x scale) (* y scale) scale scale) paint))
        (when-some [p' (map/get path p)]
          (recur p')))
      
      (.setColor paint (unchecked-int 0xFFFF0000))
      (let [{:keys [x y]} start]
        (canvas/draw-rect canvas (core/rect-xywh (* x scale) (* y scale) scale scale) paint))
      (let [{:keys [x y]} end]
        (canvas/draw-rect canvas (core/rect-xywh (* x scale) (* y scale) scale scale) paint)))

    #_(gui/redraw)
    
    (when-not (empty? @*queue)
      (swap! *queue step)
      (gui/redraw))))

(reset! gui/*app
  (ui/default-theme
    (ui/canvas
      {:on-paint on-paint})))

(gui/redraw)

(comment
  (part1))

(defn step2 [queue]
  (when-some [pos (peek queue)]
    (reduce
      (fn [queue pos']
        (cond+
          (nil? pos')
          queue
          
          (not (map/contains? terrain pos'))
          queue
          
          :let [e  (map/get terrain pos)
                e' (map/get terrain pos')]
          
          (< (+ e' 1)  e)
          queue
          
          :let [cost  (map/get costs pos')
                cost' (+ (map/get costs pos) 1)]
          
          (>= cost' cost)
          queue

          ; (= 0 (map/get terrain pos'))
          ; (empty queue)
          
          :else
          (do
            (map/set! costs pos' cost')
            (map/set! path pos' pos)
            (conj queue pos'))))
      (pop queue)
      (v2/neighbours4 pos))))

(defn part2 []
  (map/set! costs end 0)
  (swap! *queue2 #(-> % empty (conj end)))
  (while (not-empty @*queue2)
    (swap! *queue2 step2))
  (reduce min 9999
    (for [y (range (:h terrain))
          x (range (:w terrain))
          :let [e (map/get terrain (v2 x y))
                c (map/get costs (v2 x y))]
          :when (and (= e 0) (< c 9999))]
      c)))

(comment
  (swap! *queue2 step2)
  (part2))

(comment
  (rr/from "https://github.com/weavejester/medley/master/src/medley/core.cljc"
    :require [assoc-some])
  
  (require
    '[advent-of-code.year2022.gui :as gui]
    '[io.github.humbleui.ui :as ui])

  (reset! gui/*app
    (ui/default-theme
      (ui/focus-controller
        (ui/center
          (ui/label "Day 12")))))

  (gui/redraw))
