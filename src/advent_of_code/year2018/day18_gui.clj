(ns advent-of-code.year2018.day18-gui
  (:require
   [advent-of-code.core :refer [cond+]]
   [advent-of-code.year2018.day18 :as day18]
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

(def *chart
  (atom (day18/parse day18/example)))

(add-watch *chart :redraw
  (fn [_ _ _ chart]
    (window/request-frame @*window)))

(defn on-paint [window ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFEEEEEE))
  (let [chart       @*chart
        bounds      (.getContentRect (window/jwm-window window))
        pixel-size  (min
                      (quot (.getWidth bounds) (:width chart))
                      (quot (.getHeight bounds) (:height chart)))]
    (.translate canvas
      (-> (.getWidth bounds) (- (* (:width chart) pixel-size)) (quot 2))
      (-> (.getHeight bounds) (- (* (:height chart) pixel-size)) (quot 2)))
    (.scale canvas pixel-size pixel-size)
    (with-open [paint (Paint.)]
      (doseq [y (range (:height chart))
              x (range (:width chart))
              :let [ch (day18/char-at chart [x y])]]
        (.setColor paint
          (condp = ch
            day18/GROUND (unchecked-int 0xFFdda15e)
            day18/TREE   (unchecked-int 0xFF606c38)
            day18/LUMBER (unchecked-int 0xFF283618)))
        (.drawRect canvas (Rect/makeXYWH x y 1 1) paint)))))

(comment
  (window/request-frame @*window))

(defn make-window []
  (let [w (window/make
            {:on-close (fn [_] (reset! *window nil))
             :on-paint #'on-paint})]
    (window/set-title w "Settlers of The North Pole")
    (window/set-window-position w 2024 526)
    (window/set-content-size w 1000 1000)
    (window/set-z-order w :floating)
    (window/set-visible w true)
    (window/request-frame w)
    w))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2018.day18 :reload-all))

  (do
    (hui/doui (window/close @*window))
    (reset! *window (hui/doui (make-window))))

  (window/request-frame @*window)
  (window/set-window-position @*window 2024 526)
  (window/set-content-size @*window 1000 1056)
  (window/get-window-rect @*window)

  (reset! *chart (day18/parse day18/example))
  (reset! *chart (day18/parse day18/problem))

  (reset! *chart {:width  100
                  :height 100
                  :array  (repeatedly 10000
                            #(rand-nth [day18/GROUND
                                        day18/GROUND
                                        day18/GROUND
                                        day18/TREE
                                        day18/LUMBER]))})
  (loop []
    (swap! *chart day18/step)
    (Thread/sleep 16)
    (recur))
)
