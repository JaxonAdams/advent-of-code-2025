(ns aoc.problems.day10
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

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

(defn- button-press-combinations [buttons]
  (->> buttons
       count
       inc
       (range 1)
       (mapcat #(combo/combinations buttons %))))

(defn- press-buttons [initial-light-diagram buttons]
  (reduce (fn [light-diagram-updated light-idx]
            (->> (nth light-diagram-updated light-idx)
                 not
                 (assoc light-diagram-updated light-idx)))
          (vec initial-light-diagram)
          (flatten buttons)))

(defn- min-btn-presses [light-diagram buttons]
  (let [initial-state (repeat (count light-diagram) false)
        button-combos (button-press-combinations buttons)]
    (->> button-combos
         (filter #(= light-diagram (press-buttons initial-state %)))
         (sort-by count)
         first
         count)))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (parse-instruction "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

  (press-buttons [false true true false] [[1 3] [3]])

  (press-buttons [false false false false false] [[0 4] [0 1 2] [1 2 3 4]])

  (button-press-combinations [[3] [1 3] [2] [2 3] [0 2] [0 1]])

  ;; 2
  (min-btn-presses [false true true false] [[3] [1 3] [2] [2 3] [0 2] [0 1]])
  ;; 3
  (min-btn-presses [false false false true false] [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]])
  ;; 2
  (min-btn-presses [false true true true false true] [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]]))

