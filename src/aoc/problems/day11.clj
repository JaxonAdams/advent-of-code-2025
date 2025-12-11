(ns aoc.problems.day11
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

(defn- parse-graph [map-lines]
  (reduce
   (fn [graph line-str]
     (let [[start-str nexts-str] (string/split line-str #":")
           start-key (keyword start-str)
           nexts (-> nexts-str
                     string/trim
                     (string/split #" ")
                     (->> (map keyword)))]
       (assoc graph start-key nexts)))
   {}
   map-lines))

(defn- dfs-find-paths
  [graph start target]
  (letfn [(dfs [current visited current-path]
            (cond (= current target) [current-path]
                  (visited current) []
                  :else (->> (get graph current [])
                             (map #(dfs % (conj visited current) (conj current-path %)))
                             (reduce concat))))]
    (dfs start #{} [])))

(defn count-paths [graph start]
  (->> (dfs-find-paths graph start :out)
       count))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (def example-map ["aaa: you hhh"
                    "you: bbb ccc"
                    "bbb: ddd eee"
                    "ccc: ddd eee fff"
                    "ddd: ggg"
                    "eee: out"
                    "fff: out"
                    "ggg: out"
                    "hhh: ccc fff iii"
                    "iii: out"])

  (parse-graph example-map)

  ;; 5
  (-> example-map
      parse-graph
      (count-paths :you)))

;; For the solution...
(comment
  (-> "input/day11/input.txt"
      read-file-lines
      parse-graph
      (count-paths :you)))

;; ----------------------------------------------------------------------------
;; PART TWO

(comment

  (def example-map-2 ["svr: aaa bbb"
                      "aaa: fft"
                      "fft: ccc"
                      "bbb: tty"
                      "tty: ccc"
                      "ccc: ddd eee"
                      "ddd: hub"
                      "hub: fff"
                      "eee: dac"
                      "dac: fff"
                      "fff: ggg hhh"
                      "ggg: out"
                      "hhh: out"])

  ;; 2
  (-> example-map-2
      parse-graph
      (dfs-find-paths :svr :out)))
