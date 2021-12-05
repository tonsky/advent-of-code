(ns advent-of-code.year2018.day16
  (:require
   [clojure.string :as str]))

(defn parse-example [example]
  (let [[b0 b1 b2 b3 op a b c a0 a1 a2 a3]
        (->> example
            (re-find #"Before: \[(\d+), (\d+), (\d+), (\d+)\]\n(\d+) (\d+) (\d+) (\d+)\nAfter:\s*\[(\d+), (\d+), (\d+), (\d+)\]")
            (next)
            (mapv parse-long))]
    {:before [b0 b1 b2 b3]
     :command [op a b c]
     :after [a0 a1 a2 a3]}))

(def input1
  (-> (slurp "inputs/year2018/day16.part1")
    (str/split #"\n\n")
    (->> (mapv parse-example))))

(defn addr [regs [_ a b c]]
  (assoc regs c (+ (nth regs a) (nth regs b))))

(defn addi [regs [_ a b c]]
  (assoc regs c (+ (nth regs a) b)))

(defn mulr [regs [_ a b c]]
  (assoc regs c (* (nth regs a) (nth regs b))))

(defn muli [regs [_ a b c]]
  (assoc regs c (* (nth regs a) b)))

(defn banr [regs [_ a b c]]
  (assoc regs c (bit-and (nth regs a) (nth regs b))))

(defn bani [regs [_ a b c]]
  (assoc regs c (bit-and (nth regs a) b)))

(defn borr [regs [_ a b c]]
  (assoc regs c (bit-or (nth regs a) (nth regs b))))

(defn bori [regs [_ a b c]]
  (assoc regs c (bit-or (nth regs a) b)))

(defn setr [regs [_ a b c]]
  (assoc regs c (nth regs a)))

(defn seti [regs [_ a b c]]
  (assoc regs c a))

(defn gtir [regs [_ a b c]]
  (assoc regs c (if (> a (nth regs b)) 1 0)))

(defn gtri [regs [_ a b c]]
  (assoc regs c (if (> (nth regs a) b) 1 0)))

(defn gtrr [regs [_ a b c]]
  (assoc regs c (if (> (nth regs a) (nth regs b)) 1 0)))

(defn eqir [regs [_ a b c]]
  (assoc regs c (if (= a (nth regs b)) 1 0)))

(defn eqri [regs [_ a b c]]
  (assoc regs c (if (= (nth regs a) b) 1 0)))

(defn eqrr [regs [_ a b c]]
  (assoc regs c (if (= (nth regs a) (nth regs b)) 1 0)))

(def ops [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn part1 []
  (count
    (for [{:keys [before command after]} input1
          :let [fitting (filter #(= (% before command) after) ops)]
          :when (>= (count fitting) 3)]
      true)))

(def input2
  (-> (slurp "inputs/year2018/day16.part2")
    (str/split #"\n")
    (->> (mapv #(-> % (str/split #" ") (->> (mapv parse-long)))))))

(defn without [xs x]
  (remove #(= % x) xs))

(defn part2 []
  (let [all-ops (into {} (map #(vector % ops) (range 16)))
        fitting (reduce
                  (fn [opcodes {:keys [before command after]}]
                    (let [[op _ _ _] command
                          ops  (opcodes op)
                          ops' (filter #(= (% before command) after) ops)]
                      (assoc opcodes op ops')))
                  all-ops
                  input1)
        certain (fn [ops]
                  (some
                    (fn [[op op-fns]]
                      (when (= (count op-fns) 1)
                        [op (first op-fns)]))
                    ops))
        ops     (loop [ops fitting
                       res {}]
                  (if (empty? ops)
                    res
                    (let [[op op-fn] (certain ops)]
                      (recur
                        (-> ops (dissoc op) (update-vals #(without % op-fn)))
                        (assoc res op op-fn)))))
        regs    (reduce
                  (fn [regs [op a b c :as command]]
                    ((ops op) regs command))
                  [0 0 0 0]
                  input2)]
    (first regs)))

(comment
  (part1)
  (part2))