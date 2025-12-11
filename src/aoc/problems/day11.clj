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

(defn count-simple-paths [graph start end]
  (letfn [(dfs [current visited]
            (cond
              (= current end) 1
              (visited current) 0
              :else
              (->> (get graph current [])
                   (map #(dfs % (conj visited current)))
                   (reduce +))))]
    (dfs start #{})))

(defn count-paths-via-nodes [graph start end via-nodes]
  (let [[node1 node2] via-nodes
        futures [(future (count-simple-paths graph start node1))
                 (future (count-simple-paths graph node1 node2))
                 (future (count-simple-paths graph node2 end))
                 (future (count-simple-paths graph start node2))
                 (future (count-simple-paths graph node2 node1))
                 (future (count-simple-paths graph node1 end))]
        [p1 p2 p3 p4 p5 p6] (map deref futures)]
    (+ (* p1 p2 p3) (* p4 p5 p6))))

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
      (count-simple-paths :you :out)))

;; For the solution...
(comment
  (-> "input/day11/input.txt"
      read-file-lines
      parse-graph
      (count-simple-paths :you :out)))

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
      (count-paths-via-nodes :svr :out [:dac :fft])))

;; For the solution...
(comment
  (-> "input/day11/input.txt"
      read-file-lines
      parse-graph
      (count-paths-via-nodes :svr :out [:dac :fft])))
