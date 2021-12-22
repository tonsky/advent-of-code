(ns advent-of-code.year2021.day8
  (:require
   [clojure.string :as str]))

(defn deduce [digits]
  (let [numbers {[0 1 2 4 5 6]   0
                 [2 5]           1
                 [0 2 3 4 6]     2
                 [0 2 3 5 6]     3
                 [1 2 3 5]       4
                 [0 1 3 5 6]     5
                 [0 1 3 4 5 6]   6 
                 [0 2 5]         7
                 [0 1 2 3 4 5 6] 8
                 [0 1 2 3 5 6]   9}
        decode  (fn [digit code]
                  (vec (sort (map #(nth code (- (int %) (int \a))) digit))))
        code    (first
                  (for [a (range 7)
                        b (range 7)
                        :when (not= a b)
                        c (range 7)
                        :when (not (#{a b} c))
                        d (range 7)
                        :when (not (#{a b c} d))
                        e (range 7)
                        :when (not (#{a b c d} e))
                        f (range 7)
                        :when (not (#{a b c d e} f))
                        g (range 7)
                        :when (not (#{a b c d e f} g))
                        :let [code   [a b c d e f g]
                              result (map #(decode % code) digits)]
                        :when (every? numbers result)]
                    code))]
    (into {}
      (for [digit digits]
        [digit (numbers (decode digit code))]))))

(defn join-digits [ds]
  (reduce (fn [acc d] (+ (* acc 10) d)) 0 ds))

(defn main [& args]
  (let [lines (str/split-lines (slurp "inputs/year2021/day8"))]
    (println
      (count
        (for [line lines
              :let [[_ output] (str/split line #"\|")]
              digit (str/split (str/trim output) #"\s+")
              :when (#{2 3 4 7} (count digit))]
          digit)))
    (println
      (reduce +
        (for [line lines
              :let [[digits output] (str/split line #"\s+\|\s+")
                    digits (->> (str/split digits #"\s+")
                             (mapv #(str/join (sort %))))
                    code   (deduce digits)
                    output (->> (str/split output #"\s+")
                             (mapv #(str/join (sort %)))
                             (mapv code)
                             join-digits)]]
         output)))))

(comment (main))