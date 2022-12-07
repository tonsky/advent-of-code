(ns advent-of-code.year2022.dayXX
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(def sample1
  "")

(def data
  (slurp (io/file "inputs/year2022/dayXX")))

(defn parse [data]
  data)

(defn part1 [data]
  (let [data (parse data)]
    ))

(comment
  (part1 sample1)
  (part1 data))

(defn parse2 [data]
  (parse data))

(defn part2 [data]
  (let [data (parse2 data)]
    ))

(comment
  (part2 sample1)
  (part2 data))

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
          (ui/label "Day XX")))))

  (gui/redraw))
