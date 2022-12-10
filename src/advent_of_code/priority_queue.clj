(ns advent-of-code.priority-queue
  (:refer-clojure :exclude [conj disj peek pop seq])
  (:require
    [clojure.data.priority-map :as priority-map]))

(defn priority-queue-by [cost-fn & xs]
  (with-meta
    (into (priority-map/priority-map)
      (for [x xs]
        [x (cost-fn x)]))
    {:cost-fn cost-fn}))
    
(defn conj [m x]
  (assoc m x ((:cost-fn (meta m)) x)))

(defn disj [m x]
  (dissoc m x))

(defn pop [m]
  (clojure.core/pop m))

(defn peek [m]
  (first (clojure.core/peek m)))

(defn seq [m]
  (keys m))