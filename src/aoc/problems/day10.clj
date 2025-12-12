(ns aoc.problems.day10
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [utils.files :refer [read-file-lines]]))

;; Note: It's messy, but it works ¯\_(ツ)_/¯
;; I'll come back and clean it up later

(defn- parse-indicator-light-diagram [diagram-str]
  (->> diagram-str
       rest
       butlast
       (map
        (fn [c]
          (cond
            (= \. c) false
            (= \# c) true)))))

(defn- parse-button-wiring-schematics [buttons]
  (map #(->> (re-seq #"\d+" %)
             (map Integer/parseInt)) buttons))

(defn- parse-instruction [instruction]
  (let [parts (string/split instruction #" ")
        indicator-light-diagram (parse-indicator-light-diagram (first parts))
        button-wiring-schematics (parse-button-wiring-schematics (-> parts rest butlast))]
    {:light-diagram indicator-light-diagram
     :button-schematics button-wiring-schematics}))

(defn- button-press-combinations [buttons n]
  (let [with-n-repeats (mapcat (partial repeat n) buttons)]
    (->> with-n-repeats
         count
         inc
         (range 1)
         (mapcat #(combo/combinations with-n-repeats %)))))

(defn- press-buttons [initial-state buttons update-fn]
  (reduce (fn [new-state diagram-idx]
            (->> (nth new-state diagram-idx)
                 update-fn
                 (assoc new-state diagram-idx)))
          (vec initial-state)
          (flatten buttons)))

(defn- min-btn-presses-for-lights [desired-state-diagram buttons n-combos update-fn initial-value]
  (let [initial-state (repeat (count desired-state-diagram) initial-value)
        button-combos (button-press-combinations buttons n-combos)]
    (->> button-combos
         (filter #(= desired-state-diagram (press-buttons initial-state % update-fn)))
         (sort-by count)
         first
         count)))

(defn- fewest-presses-required-light-config [instruction-strs]
  (let [instructions (map parse-instruction instruction-strs)]
    (->> instructions
         (map (fn [{:keys [light-diagram button-schematics]}]
                (min-btn-presses-for-lights light-diagram button-schematics 1 not false)))
         (reduce +))))

;; Credit where credit is due --
;; I originally tried to implement a Meet-in-the-Middle approach, but it took
;; too long to run. I got this idea from the following reddit post:
;; https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/

(def INF ##Inf)

(defn parse-joltage-diagram-part-2 [diagram-str]
  (-> diagram-str
      rest
      butlast
      (->> (apply str))
      (string/split #",")
      (->> (map #(Integer/parseInt (string/trim %)))) ; Add trim for robustness
      (vec))) ; The joltages must be a vector for consistent caching keys

(defn parse-button-wiring-schematics-part-2 [button-strings]
  (->> button-strings
       (map #(->> (re-seq #"\d+" %)
                  (map Integer/parseInt)
                  set)) ; Convert indices to sets for efficient lookups
       (vec))) ; Ensure the list of button sets is a vector

(defn parse-instruction-part-2 [instruction]
  (let [parts (string/split instruction #"\s+") ; Use one or more spaces as delimiter
        button-strings (-> parts rest butlast)
        button-schematics (parse-button-wiring-schematics-part-2 button-strings)
        joltage-diagram (parse-joltage-diagram-part-2 (last parts))]
    {:buttons button-schematics ; Vector of sets
     :joltages joltage-diagram})) ; Vector of integers

(defn- calculate-joltages-count
  [presses buttons num-joltages]
  (vec
   (for [i (range num-joltages)] ; Target joltage index
     (->> (map-indexed
           (fn [j button-set]
             (if (and (= 1 (nth presses j))
                      (contains? button-set i))
               1
               0))
           buttons)
          (reduce + 0)))))

(defn- find-parity-matches [buttons target-parity]
  (let [num-buttons (count buttons)
        num-joltages (count target-parity)]

    (filter
     (fn [presses]
       (let [counts (calculate-joltages-count presses buttons num-joltages)]
         (every? true?
                 (map (fn [count target] (= (mod count 2) target))
                      counts
                      target-parity))))
     (for [i (range (int (Math/pow 2 num-buttons)))]
       (vec
        (for [j (range num-buttons)]
          (-> i (quot (int (Math/pow 2 j))) (mod 2))))))))

(declare solve-recursively) ; Forward declaration for memoized function

(def memoized-solve-recursively
  (memoize
   (fn [buttons target-joltages]
     (solve-recursively buttons target-joltages))))

(defn- solve-recursively [buttons target-joltages]
  ;; Base Case: Target is all zeros
  (if (every? zero? target-joltages)
    0

    ;; Pruning/Invalid Check: No negative joltages are allowed
    (if (some neg? target-joltages)
      INF

      (let [num-joltages (count target-joltages)

            ;; 1. Determine the required parity (1 for odd, 0 for even)
            target-parity (vec (map #(mod % 2) target-joltages))

            ;; 2. Find all minimal button press patterns (0s and 1s) that match the parity
            matching-press-patterns (find-parity-matches buttons target-parity)]

        (if (empty? matching-press-patterns)
          ;; If no combination of 0/1 presses can achieve the target parity, it's impossible.
          INF

          ;; 3. Recursive Step: Evaluate the cost for each matching pattern
          (let [min-presses
                (apply min
                       (for [pattern matching-press-patterns]
                         (let [;; Cost from the 0/1 presses in this pattern
                               pattern-cost (reduce + pattern)

                               ;; Calculate the remaining joltage required
                               ;; remaining = target - pattern_presses
                               remaining-joltages (vec
                                                   (map - target-joltages (calculate-joltages-count pattern buttons num-joltages)))]

                               ;; Check for the "even number" condition
                           (if (not (every? even? remaining-joltages))
                             INF ; Should not happen if Part 1 logic is correct, but safe check

                                 ;; Divide the remaining joltages by 2 and recurse
                             (let [next-joltages (vec (map #(quot % 2) remaining-joltages))

                                       ;; Total cost: 2 * f(next) + pattern_cost
                                   recursive-cost (memoized-solve-recursively buttons next-joltages)]

                                   ;; Check if the recursive call found a solution
                               (if (= recursive-cost INF)
                                 INF ; If sub-problem is impossible, this path is impossible

                                     ;; Final Cost
                                 (+ (* 2 recursive-cost) pattern-cost)))))))]

            min-presses))))))

(defn solve-part-2 [instruction]
  (let [{:keys [buttons joltages]} (parse-instruction-part-2 instruction)]
    (memoized-solve-recursively buttons joltages)))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (parse-instruction-part-2 "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

  (press-buttons [false true true false] [[1 3] [3]] not)

  (press-buttons [false false false false false] [[0 4] [0 1 2] [1 2 3 4]] not)

  (button-press-combinations [[3] [1 3] [2] [2 3] [0 2] [0 1]] 1)

  ;; 2
  (min-btn-presses-for-lights [false true true false] [[3] [1 3] [2] [2 3] [0 2] [0 1]] 1 not false)
  ;; 3
  (min-btn-presses-for-lights [false false false true false] [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] 1 not false)
  ;; 2
  (min-btn-presses-for-lights [false true true true false true] [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] 1 not false)

  (def example-instructions ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                             "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                             "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"])

  ;; 7
  (fewest-presses-required-light-config example-instructions))

;; For the solution...
(comment
  (-> "input/day10/input.txt"
      read-file-lines
      fewest-presses-required-light-config))

;; ----------------------------------------------------------------------------
;; PART TWO

;; For the solution...
(comment

  ;; 33
  (->> example-instructions
       (map solve-part-2)
       (reduce +)))

;; For the solution...
(comment
  (->> "input/day10/input.txt"
       read-file-lines
       (map solve-part-2)
       (reduce +)))
