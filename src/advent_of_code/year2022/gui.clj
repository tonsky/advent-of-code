(ns advent-of-code.year2022.gui
  (:require
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.window :as window]))

(defonce *app
  (atom
    (ui/default-theme
      (ui/focus-controller
        (ui/center
          (ui/label "Hello Adventers!"))))))

(defonce *window
  (atom nil))

(defn redraw []
  (some-> @*window window/request-frame)
  :redraw)

(ui/start-app!
  (reset! *window 
    (ui/window
      {:title "🌲 🌲 🌲 Advent of Code 2022 🌲 🌲 🌲"}
      *app)))

(redraw)