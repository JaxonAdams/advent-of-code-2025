(ns aoc.problems.day10
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [utils.files :refer [read-file-lines]]))

;; TODO: clean up this code!

(defn- parse-joltage-diagram [diagram-str]
  (-> diagram-str
      rest
      butlast
      (->> (apply str))
      (string/split #",")
      (->> (map Integer/parseInt))))

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
        button-wiring-schematics (parse-button-wiring-schematics (-> parts rest butlast))
        joltage-diagram (parse-joltage-diagram (last parts))]
    {:light-diagram indicator-light-diagram
     :button-schematics button-wiring-schematics
     :joltage-diagram joltage-diagram}))

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

(defn- min-btn-presses [desired-state-diagram buttons n-combos update-fn initial-value]
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
                (min-btn-presses light-diagram button-schematics 1 not false)))
         (reduce +))))

(defn- fewest-presses-required-joltage-config [instruction-strs]
  (let [instructions (map parse-instruction instruction-strs)]
    (->> instructions
         (map (fn [{:keys [joltage-diagram button-schematics]}]
                (min-btn-presses joltage-diagram button-schematics (apply max joltage-diagram) inc 0)))
         (reduce +))))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (parse-instruction "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

  (press-buttons [false true true false] [[1 3] [3]] not)

  (press-buttons [false false false false false] [[0 4] [0 1 2] [1 2 3 4]] not)

  (button-press-combinations [[3] [1 3] [2] [2 3] [0 2] [0 1]] 1)

  ;; 2
  (min-btn-presses [false true true false] [[3] [1 3] [2] [2 3] [0 2] [0 1]] 1 not false)
  ;; 3
  (min-btn-presses [false false false true false] [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] 1 not false)
  ;; 2
  (min-btn-presses [false true true true false true] [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] 1 not false)

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

(comment

  ;; 10
  (min-btn-presses [3 5 4 7] [[3] [1 3] [2] [2 3] [0 2] [0 1]] 7 inc 0)

  ;; 33
  (fewest-presses-required-joltage-config example-instructions))
