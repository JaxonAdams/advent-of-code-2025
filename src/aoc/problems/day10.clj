(ns aoc.problems.day10
  (:require [clojure.string :as string]))

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

(defn- press-button [initial-light-diagram button]
  (reduce (fn [light-diagram-updated light-idx]
            (->> (nth light-diagram-updated light-idx)
                 not
                 (assoc light-diagram-updated light-idx)))
          initial-light-diagram
          button))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (parse-instruction "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

  (press-button [false true true false] [1 3]))

