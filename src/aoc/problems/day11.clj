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

(defn count-paths-floyd-warshall [graph]
  (let [all-nodes (into (set (keys graph))
                        (mapcat val graph))
        nodes (vec all-nodes)
        node-to-idx (zipmap nodes (range))
        n (count nodes)
        ;; Initialize adjacency matrix
        matrix (reduce
                (fn [m [from tos]]
                  (let [from-idx (node-to-idx from)]
                    (reduce
                     (fn [m2 to]
                       (let [to-idx (node-to-idx to)]
                         (assoc-in m2 [from-idx to-idx] 1)))
                     m tos)))
                (vec (repeat n (vec (repeat n 0))))
                graph)]
    ;; Floyd-Warshall with path counting
    (loop [d matrix, k 0]
      (if (< k n)
        (recur
         (reduce
          (fn [d2 i]
            (reduce
             (fn [d3 j]
               (update-in d3 [i j] + (* (get-in d2 [i k]) (get-in d2 [k j]))))
             d2 (range n)))
          d (range n))
         (inc k))
        ;; Return function to get path count between nodes
        (fn [start end]
          (let [start-idx (node-to-idx start)
                end-idx (node-to-idx end)]
            (get-in d [start-idx end-idx] 0)))))))

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
  (let [graph (parse-graph example-map-2)
        path-counter (count-paths-floyd-warshall graph)]
    (* (path-counter :svr :fft)
       (path-counter :fft :dac)
       (path-counter :dac :out))))

;; For the solution...
(comment
  (let [graph (-> "input/day11/input.txt" read-file-lines parse-graph)
        path-counter (count-paths-floyd-warshall graph)]
    (* (path-counter :svr :fft)
       (path-counter :fft :dac)
       (path-counter :dac :out))))
