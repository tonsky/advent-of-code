(ns advent-of-code.year2018.day15-gui
  (:require
   [advent-of-code.core :refer [cond+]]
   [advent-of-code.year2018.day15 :as day15]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.window :as window]
   [nrepl.cmdline :as nrepl])
  (:import
   [advent_of_code.year2018.day15 Pos Unit Game]
   [java.nio.file Files Path]
   [java.util Arrays BitSet]
   [io.github.humbleui.skija Canvas Font Image Paint Rect]))

(defn slurp-bytes [path]
  (Files/readAllBytes
    (Path/of path (make-array String 0))))

(def sprite-floor
  (Image/makeFromEncoded (slurp-bytes "inputs/year2018/day15/floor.png")))

(def sprite-wall
  (Image/makeFromEncoded (slurp-bytes "inputs/year2018/day15/wall.png")))

(def sprite-goblin
  (Image/makeFromEncoded (slurp-bytes "inputs/year2018/day15/goblin.png")))

(def sprite-elf
  (Image/makeFromEncoded (slurp-bytes "inputs/year2018/day15/elf.png")))

(def font
  (Font. nil (float 26)))

(defonce *window
  (atom nil))

(def *game
  (atom (day15/parse day15/example1)))

(defn on-paint [window ^Canvas canvas game]
  (.clear canvas (unchecked-int 0xFFEEEEEE))
  (let [bounds      (.getContentRect (window/jwm-window window))
        sprite-size (math/floor
                      (min
                        (/ (.getWidth bounds) (:width game))
                        (/ (.getHeight bounds) (:height game))))
        sprite-rect (Rect/makeXYWH 0 0 sprite-size sprite-size)
        draw-unit   (fn [unit]
                      (case (:who unit)
                        :goblin (.drawImageRect canvas sprite-goblin sprite-rect)
                        :elf    (.drawImageRect canvas sprite-elf sprite-rect))
                      (with-open [paint (Paint.)]
                        (let [width (* (:hp unit) (- sprite-size 4) (/ 1 200))]
                          (.setColor paint (unchecked-int 0xFF33CC33))
                          (.drawRect canvas (Rect/makeXYWH 2 0 width 6) paint)
                          (.setColor paint (unchecked-int 0xFFCC3333))
                          (.drawRect canvas (Rect/makeXYWH (+ width 2) 0 (- sprite-size 4 width) 6) paint)
                          (.setColor paint (unchecked-int 0xFFFFFFFF))
                          #_(.drawString canvas (str (:hp unit)) 0 20 font paint))))]
    (.translate canvas
      (-> (.getWidth bounds) (- (* (:width game) sprite-size)) (quot 2))
      (-> (.getHeight bounds) (- (* (:height game) sprite-size)) (quot 2)))
    (doseq [y    (range 0 (:height game))
            x    (range 0 (:width game))
            :let [obj (day15/game-get game (Pos. x y))]]
      (.save canvas)
      (.translate canvas (* x sprite-size) (* y sprite-size))
      (.drawImageRect canvas sprite-floor sprite-rect)
      (cond
        (= day15/wall obj) (.drawImageRect canvas sprite-wall sprite-rect)
        (some? obj)   (draw-unit obj))
      (.restore canvas))))

(comment
  (window/request-frame @*window))

(defn make-window []
  (let [w (window/make
            {:on-close (fn [_] (reset! *window nil))
             :on-paint (fn [window ^Canvas canvas]
                         (let [layer (.save canvas)]
                           (try
                             (let [on-paint (resolve `on-paint)
                                   *game    (resolve `*game)]
                               (@on-paint window canvas @@*game)
                               (window/request-frame window))
                             (catch Exception e
                               (.printStackTrace e)
                               (.clear canvas (unchecked-int 0xFFCC3333))))
                           (.restoreToCount canvas layer)))})]
    (window/set-title w "Goblins vs Elves")
    (.setContentSize (window/jwm-window w) 1000 1000)
    ; (.setWindowPosition (window/jwm-window w) 2836 632)
    ; (.setWindowSize (window/jwm-window w) 1000 1588)
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
  (hui/doui (window/close @*window))
  (reset! *window (hui/doui (make-window)))
  (window/request-frame @*window)
  (hui/doui (window/set-z-order @*window :normal))
  (hui/doui (window/set-z-order @*window :floating))

  (swap! *game day15/make-turn)
  (reset! *game (day15/parse day15/example2))
  (day15/part1 day15/problem {:delay 100 :on-turn #(reset! *game %)})

  (day15/part2 day15/problem {:on-turn #(reset! *game %)}))