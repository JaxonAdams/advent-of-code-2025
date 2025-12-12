(ns aoc.problems.day12
  (:require [aoc.utils.files :refer [read-file-lines]]
            [aoc.utils.parsing :refer [split-at-value]]
            [clojure.string :as string]))

(defn- build-shape [shape-lines]
  (letfn [(parse-line [line]
            (map #(= % \#) line))]
    (map parse-line shape-lines)))

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
    (-> parsed-input :shapes (nth 2))))
