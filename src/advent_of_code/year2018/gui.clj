(ns advent-of-code.year2018.gui
  (:require
   [io.github.humbleui.core :as hui]
   [nrepl.cmdline :as nrepl]))

(defn -main [& args]
  (future (apply nrepl/-main args))
  (hui/init)
  (hui/start))
