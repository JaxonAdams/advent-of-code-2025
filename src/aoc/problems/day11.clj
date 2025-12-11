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

(defn- visits-dac-and-fft? [path]
  (let [nodes (into #{} path)]
    (and
     (contains? nodes :dac)
     (contains? nodes :fft))))

(defn- dfs-find-paths
  [graph start target pred?]
  (let [memo (atom {})]
    (letfn [(dfs [current visited current-path]
              (let [state [current visited]]
                (if-let [cached (@memo state)]
                  cached
                  (let [result
                        (cond
                          (= current target)
                          (if (pred? current-path) 1 0)

                          (visited current)
                          0

                          :else
                          (->> (get graph current [])
                               (map #(dfs % (conj visited current) (conj current-path %)))
                               (reduce +)))]
                    (swap! memo assoc state result)
                    result))))]
      (dfs start #{} [start]))))

(defn count-paths [graph start pred?]
  (dfs-find-paths graph start :out pred?))

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
      (count-paths :you identity)))

;; For the solution...
(comment
  (-> "input/day11/input.txt"
      read-file-lines
      parse-graph
      (count-paths :you identity)))

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
      (count-paths :svr visits-dac-and-fft?)))

;; For the solution...
(comment
  (-> "input/day11/input.txt"
      read-file-lines
      parse-graph
      (count-paths :svr visits-dac-and-fft?)))
