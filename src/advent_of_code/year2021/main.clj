(ns advent-of-code.year2021.main)

(defn solve-day [day]
  (let [ns (symbol (str "advent-of-code.year2021.day" day))]
    (require ns)
    (println "Day" day)
    (print "├ part 1: ")
    (flush)
    (println ((ns-resolve ns 'part1)))
    (print "└ part 2: ")
    (flush)
    (println ((ns-resolve ns 'part2)))
    (println)))

(defn -main [& args]
  (doseq [day (range 1 (inc 11))]
    (solve-day day)))

(comment
  (solve-day 1)
  (-main))