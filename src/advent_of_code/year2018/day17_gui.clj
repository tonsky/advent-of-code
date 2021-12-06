(ns advent-of-code.year2018.day17-gui
  (:require
   [advent-of-code.core :refer [cond+]]
   [advent-of-code.year2018.day17 :as day17]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.window :as window]
   [nrepl.cmdline :as nrepl])
  (:import
   ; [advent_of_code.year2018.day17 Pos Unit Game]
   [java.nio.file Files Path]
   [java.util Arrays BitSet]
   [io.github.humbleui.skija Canvas Font Image Paint Rect]))

(defonce *window
  (atom nil))

(def *map
  (atom (day17/parse day17/example)))

(add-watch *map :redraw
  (fn [_ _ _ map]
    (window/request-frame @*window)))

(defn pixel-size [ww wh mw mh]
  (loop [p 2]
    (let [cols (inc (quot (dec (* mh p)) wh))]
      (if (> (* cols (+ mw 1) p) ww)
        (dec p)
        (recur (inc p))))))

(defn on-paint [window ^Canvas canvas map]
  (.clear canvas (unchecked-int 0xFFEEEEEE))
  (let [bounds      (.getContentRect (window/jwm-window window))
        pixel-size  (pixel-size (.getWidth bounds) (.getHeight bounds) (:width map) (:height map))
        cols        (inc (quot (dec (* (:height map) pixel-size)) (.getWidth bounds)))
        rows        (-> (:height map)
                      (/ cols)
                      (math/ceil)
                      long)]
    (.translate canvas
      (-> (.getWidth bounds) (- (* (inc (:width map)) cols pixel-size)) (quot 2))
      (-> (.getHeight bounds) (- (* rows pixel-size)) (quot 2)))
    (.scale canvas pixel-size pixel-size)
    (with-open [paint (Paint.)]
      (.setColor paint (unchecked-int 0xFFE9C46A))
      (doseq [col (range 0 cols)]
        (if (< col (dec cols))
          (.drawRect canvas (Rect/makeXYWH (* col (inc (:width map))) 0 (:width map) rows) paint)
          (.drawRect canvas (Rect/makeXYWH (* col (inc (:width map))) 0 (:width map) (inc (mod (dec (:height map)) rows))) paint)))
      (doseq [dy (range 0 (:height map))
              dx (range 0 (:width map))
              :let [y (+ dy (:ymin map))
                    x (+ dx (:xmin map))
                    val (day17/map-get map x y)]]
        (condp = val
          day17/CLAY  (.setColor paint (unchecked-int 0xFF7F5539))
          day17/WATER (.setColor paint (unchecked-int 0xFF023e8a))
          day17/DRY   (.setColor paint (unchecked-int 0xFFe6ccb2))
          nil)
        (when (not= val day17/SAND)
          (let [col (quot dy rows)
                dx' (+ dx (* col (inc (:width map))))
                dy' (mod dy rows)]
            (.drawRect canvas (Rect/makeXYWH dx' dy' 1 1) paint))))
      (.setColor paint (unchecked-int 0xFF3A86FF))
      (doseq [[x y] (:sources map)
              :let [dx (- x (:xmin map))
                    dy (- y (:ymin map))
                    col (quot dy rows)
                    dx' (+ dx (* col (inc (:width map))))
                    dy' (mod dy rows)]]
        (.drawRect canvas (Rect/makeXYWH (+ dx' 0.25) (+ 0.25 dy') 0.5 0.5) paint)))))

(comment
  (window/request-frame @*window))

(defn make-window []
  (let [w (window/make
            {:on-close (fn [_] (reset! *window nil))
             :on-paint (fn [window ^Canvas canvas]
                         (let [layer (.save canvas)]
                           (try
                             (let [on-paint (resolve `on-paint)
                                   *map    (resolve `*map)]
                               (@on-paint window canvas @@*map)
                               #_(window/request-frame window))
                             (catch Exception e
                               (.printStackTrace e)
                               (.clear canvas (unchecked-int 0xFFCC3333))))
                           (.restoreToCount canvas layer)))})]
    (window/set-title w "Reservoir Research")
    (.setContentSize (window/jwm-window w) 1000 1000)
    ; (.setWindowPosition (window/jwm-window w) 2836 632)
    ; (.setWindowSize (window/jwm-window w) 1320 1520)
    ; (window/set-z-order w :floating)
    (window/set-visible w true)
    (window/request-frame w)
    w))

(defn -main [& args]
  (future (apply nrepl/-main args))
  (hui/init)
  (reset! *window (make-window))
  (hui/start))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2018.day17 :reload-all))

  (hui/doui (window/close @*window))
  (reset! *window (hui/doui (make-window)))
  (window/request-frame @*window)
  (.getHeight (.getWindowRect (window/jwm-window @*window)))
  (hui/doui (window/set-z-order @*window :normal))
  (hui/doui (window/set-z-order @*window :floating))

  (reset! *map (day17/parse day17/example2))
  (swap! *map day17/step)

  (day17/part1 day17/example2
    {:on-turn #(do
                 (reset! *map %) 
                 (Thread/sleep 50))})

  (day17/part1 day17/problem
    {:on-turn #(reset! *map %)}))
