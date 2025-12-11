(ns aoc.problems.day11
  (:require [clojure.string :as string]))

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

(defn dfs-count-paths
  [graph start target]
  (letfn [(dfs [current visited]
            (cond (= current target) 1
                  (visited current) 0
                  :else (->> (get graph current [])
                             (map #(dfs % (conj visited current)))
                             (reduce +))))]
    (dfs start #{})))

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

  (-> example-map
      parse-graph
      (dfs-count-paths :you :out)))
