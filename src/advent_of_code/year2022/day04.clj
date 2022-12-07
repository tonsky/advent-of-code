{:nextjournal.clerk/visibility {:code :hide}}

(ns advent-of-code.year2022.day04
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [nextjournal.clerk :as clerk]
    [remote-require.core :as rr]))

{:nextjournal.clerk/visibility {:code :show}}

; # Day 4: Camp Cleanup

; Space needs to be cleared before the last supplies can be unloaded from the ships, and so several Elves have been assigned the job of cleaning up sections of the camp. Every section has a unique *ID number*, and each Elf is assigned a range of section IDs.

; However, as some of the Elves compare their section assignments with each other, they've noticed that many of the assignments *overlap*. To try to quickly find overlaps and reduce duplicated effort, the Elves pair up and make a *big list of the section assignments for each pair* (your puzzle input).

; For example, consider the following list of section assignment pairs:

(def sample
"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

; For the first few pairs, this list means:

; - Within the first pair of Elves, the first Elf was assigned sections `2-4` (sections `2`, `3`, and `4`), while the second Elf was assigned sections `6-8` (sections `6`, `7`, `8`).
; - The Elves in the second pair were each assigned two sections.
; - The Elves in the third pair were each assigned three sections: one got sections `5`, `6`, and `7`, while the other also got `7`, plus `8` and `9`.

; This example list uses single-digit section IDs to make it easier to draw; your actual list might contain larger numbers. Visually, these pairs of section assignments look like this:

; ```
; .234.....  2-4
; .....678.  6-8
; 
; .23......  2-3
; ...45....  4-5
; 
; ....567..  5-7
; ......789  7-9
; 
; .2345678.  2-8
; ..34567..  3-7
; 
; .....6...  6-6
; ...456...  4-6
; 
; .23456...  2-6
; ...45678.  4-8
; ```

; ## Part 1
; Some of the pairs have noticed that one of their assignments fully contains the other. For example, `2-8` fully contains `3-7`, and `6-6` is fully contained by `4-6`. In pairs where one assignment fully contains the other, one Elf in the pair would be exclusively cleaning sections their partner will already be cleaning, so these seem like the most in need of reconsideration. In this example, there are 2 such pairs.

; In how many assignment pairs does one range fully contain the other?

; ### Parse data
; Let’s write a function that parses the input first:

(defn parse [data]
  (->> data
    (re-seq #"\d+")   ;; find all numbers 
    (map parse-long)  ;; convert each string to long
    (partition 4)))   ;; group each four

; Let’s try it on sample:
^{:nextjournal.clerk/visibility {:result :show}}
(parse sample)

; ### Win condition
; We need a function that determines if one range is inside another
(defn contains? [[a b c d]]
  (or
    (<= a c d b)
    (<= c a b d)))

; Let’s check it:

; [2, 4] is not inside [6, 8]:
(contains? [2 4 6 8])

; [3, 7] is inside [2, 8]:
(contains? [2 8 3 7])

; and [6, 6] is inside [4, 6]:
(contains? [6 6 4 6])

; ### Putting it all together
; Finally, we are ready to solve the problem.
(defn part1 [data]
  (let [data (parse data)] ; parse data
    (->> data
      (filter contains?)   ; only keep pairs that match our condition
      (count))))           ; count

; Let’s test in on our sample first. We should get `2` as a result:
(part1 sample)

; and finally let’s load the real data:
(def data
  (slurp (io/file "inputs/year2022/day04")))

; and solve the problem:
(part1 data)

; ## Part 2
; It seems like there is still quite a bit of duplicate work planned. Instead, the Elves would like to know the number of pairs that overlap at all.

; In the above example, the first two pairs (`2-4,6-8` and `2-3,4-5`) don't overlap, while the remaining four pairs (`5-7,7-9`, `2-8,3-7`, `6-6,4-6`, and `2-6,4-8`) do overlap:

; - `5-7,7-9` overlaps in a single section, `7`.
; - `2-8,3-7` overlaps all of the sections `3` through `7`.
; - `6-6,4-6` overlaps in a single section, `6`.
; - `2-6,4-8` overlaps in sections `4`, `5`, and `6`.

; So, in this example, the number of overlapping assignment pairs is 4.

; In how many assignment pairs do the ranges overlap?

; ### Win condition

; A predicate for part 2 is very similar to part 1
(defn overlap? [[a b c d]]
  (or
    (<= a c b)
    (<= a d b)
    (<= c a d)
    (<= c b d)))

; Let's test it:

(overlap? [2 4 6 8])
(overlap? [2 3 4 5])
(overlap? [5 7 7 9])
(overlap? [2 8 3 7])
(overlap? [6 6 4 6])
(overlap? [2 6 4 8])

; ### Putting it all together
; Solution is pretty much the same, except that we use overlap? instead of contains?
(defn part2 [data]
  (let [data (parse data)] ; parse data
    (->> data
      (filter overlap?)    ; only keep pairs that match our condition
      (count))))           ; count

; Let’s test in on our sample first. We should get `4` as a result:
(part2 sample)

; and finally let’s solve the problem:
(part2 data)

; That’s it!

{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (clerk/serve! {:watch-paths ["src"] :browse true}))
