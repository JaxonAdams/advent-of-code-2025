(ns aoc.problems.day12
  (:require [aoc.utils.files :refer [read-file-lines]]
            [aoc.utils.parsing :refer [split-at-value]]))

(defn- build-shape [shape-lines]
  (letfn [(parse-line [line]
            (map #(= % \#) line))]
    (map parse-line shape-lines)))

(defn- rotate-shape [shape]
  (->> shape
       (apply mapv vector)
       (mapv reverse)))

(defn- mirror-shape-horizontal [shape]
  (mapv reverse shape))

(defn- mirror-shape-vertical [shape]
  (reverse shape))

(defn- get-shape-variations [shape]
  [shape
   (rotate-shape shape)
   (-> shape rotate-shape rotate-shape)
   (-> shape rotate-shape rotate-shape rotate-shape)
   (mirror-shape-vertical shape)
   (mirror-shape-horizontal shape)
   (-> shape mirror-shape-vertical mirror-shape-horizontal)])

(defn- parse-input [input-lines]
  (let [parts (split-at-value "" input-lines)
        shapes (->> parts
                    butlast
                    (map rest)
                    (map build-shape))
        regions (last parts)]
    {:shapes shapes :regions regions}))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (def example-lines (read-file-lines "input/day12/example.txt"))

  (let [parsed-input (parse-input example-lines)]
    (-> parsed-input :shapes (nth 2) get-shape-variations)))
