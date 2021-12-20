(ns advent-of-code.core)

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