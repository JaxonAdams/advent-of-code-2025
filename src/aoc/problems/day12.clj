(ns aoc.problems.day12
  (:require [aoc.utils.files :refer [read-file-lines]]
            [aoc.utils.parsing :refer [split-at-value]]
            [clojure.string :as string]))

(defn- build-shape [shape-lines]
  (letfn [(parse-line [line]
            (map #(if (= % \#) 1 0) line))]
    (map parse-line shape-lines)))

(defn- shape-area [shape]
  (->> shape
       flatten
       (reduce +)))

(defn- build-region [region-str shape-areas]
  (let [[size-str shape-counts-str] (string/split region-str #": ")
        [size-x size-y] (-> size-str (string/split #"x") (->> (map Integer/parseInt)))
        shape-counts (-> shape-counts-str (string/split #" ") (->> (map Integer/parseInt)))
        req-shape-area (->> shape-counts
                            (map-indexed (fn [idx cnt]
                                           (* cnt (nth shape-areas idx))))
                            (reduce +))]
    {:width size-x
     :height size-y
     :area (* size-x size-y)
     :req-shape-area req-shape-area}))

(defn- parse-input [input-lines]
  (let [parts (split-at-value "" input-lines)
        shapes (->> parts
                    butlast
                    (map rest)
                    (map build-shape))
        shape-areas (map shape-area shapes)
        regions (map #(build-region % shape-areas) (last parts))]
    {:shapes shapes :shape-areas shape-areas :regions regions}))

;; After looking at the input, it looks like I can set an upper bound like this...
(defn- count-can-fit-in-region [regions]
  (->> regions
       (filter #(< (:req-shape-area %) (:area %)))
       count))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (def example-lines (read-file-lines "input/day12/example.txt"))

  (let [parsed-input (parse-input example-lines)]
    (-> parsed-input :regions)))

;; For the solution...
(comment
  (-> "input/day12/input.txt"
      read-file-lines
      parse-input
      :regions
      count-can-fit-in-region))
