(ns advent-of-code.core
  (:require
    [clj-async-profiler.core :as prof]))

(defn abs ^long [^long x]
  (if (< x 0)
    (- x)
    x))

(defmacro cond+ [& clauses]
  (when-some [[test expr & rest] clauses]
    (case test
      :do   `(do ~expr (cond+ ~@rest))
      :let  `(let ~expr (cond+ ~@rest))
      :some `(or ~expr (cond+ ~@rest))
            `(if ~test ~expr (cond+ ~@rest)))))

(defmacro forv [& forms]
  `(vec (for ~@forms)))

(defmacro measure [& body]
  (let [[opts body] (if (map? (first body))
                      [(first body) (next body)]
                      [{} body])]
    `(let [t0#  (System/nanoTime)
           res# (if (:profile ~opts)
                  (prof/profile ~@body)
                  (do ~@body))
           dt#  (-> (- (System/nanoTime) t0#)
                  (/ 1000000)
                  (double))]
       (if (map? res#)
         (assoc res# :time-ms dt#)
         {:time-ms dt#, :res res#}))))

(defn zip [& xs]
  (apply map vector xs))

(defn now []
  (System/currentTimeMillis))

(defn clamp [x from to]
  (min (max x from) to))

(defn between? [x from to]
  (and
    (<= from x)
    (< x to)))