(ns advent-of-code.year2021.day25-gui
  (:require
   [advent-of-code.core :refer [cond+ forv]]
   [advent-of-code.year2021.day25 :as day25]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.window :as window]
   [nrepl.cmdline :as nrepl])
  (:import
   [io.github.humbleui.skija Canvas Font Image Paint Rect]))

(defonce *window
  (atom nil))

(def *state
  (atom (day25/parse day25/example)))

(add-watch *state :redraw
  (fn [_ _ _ state]
    (window/request-frame @*window)))

(defn on-paint [window ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFEEEEEE))
  (let [state       @*state
        bounds      (.getContentRect (window/jwm-window window))
        width       (count (first state))
        height      (count state)
        pixel-size  (min
                      (quot (.getWidth bounds) width)
                      (quot (.getHeight bounds) height))]
    (.translate canvas
      (-> (.getWidth bounds) (- (* width pixel-size)) (quot 2))
      (-> (.getHeight bounds) (- (* height pixel-size)) (quot 2)))
    (.scale canvas pixel-size pixel-size)
    (with-open [paint (Paint.)]
      (doseq [y (range height)
              x (range width)
              :let [ch (nth (nth state y) x)]]
        (.setColor paint
          (condp = ch
            \. (unchecked-int 0xFF03045e)
            \> (unchecked-int 0xFF00b4d8)
            \v (unchecked-int 0xFFcaf0f8)))
        (.drawRect canvas (Rect/makeXYWH x y 1 1) paint)))))

(comment
  (window/request-frame @*window))

(defn make-window []
  (let [w (window/make
            {:on-close (fn [_] (reset! *window nil))
             :on-paint #'on-paint})]
    (window/set-title w "Sea Cucumber")
    (window/set-window-position w 2024 526)
    (window/set-content-size w 1000 1000)
    (window/set-z-order w :floating)
    (window/set-visible w true)
    (window/request-frame w)
    w))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day25 :reload-all))

  (do
    (hui/doui (window/close @*window))
    (reset! *window (hui/doui (make-window))))

  (window/request-frame @*window)
  (window/set-window-position @*window 2024 526)
  (window/set-content-size @*window 1000 1056)
  (window/get-window-rect @*window)

  (reset! *state (day25/parse day25/example))
  (reset! *state (day25/parse day25/input))

  (defn random-input []
    (forv [y (range 137)]
      (forv [x (range 137)]
        (rand-nth [\. \. \. \> \v]))))

  (let [delay 16]
    (reset! *state (random-input) #_(day25/parse day25/input))
    (loop []
      (let [state   @*state
            state'  (swap! *state day25/move-right)
            _       (Thread/sleep delay)
            state'' (swap! *state day25/move-down)
            _       (Thread/sleep delay)]
        (when (not= state state'')
          (recur)))))
)
