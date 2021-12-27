(ns advent-of-code.gui
  (:require
   [io.github.humbleui.core :as hui]
   [nrepl.cmdline :as nrepl]))

(defn -main [& args]
  (future (apply nrepl/-main args))
  (hui/init)
  (hui/start))
