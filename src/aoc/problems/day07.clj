(ns aoc.problems.day07
  (:require [utils.files :refer [read-file-lines]]
            [clojure.string :as string]))

(defn- diagram-rows->matrix [diagram-rows]
  (mapv #(string/split % #"") diagram-rows))

(defn- get-next-cell [row-idx col-idx diagram-matrix]
  (let [num-cols (-> diagram-matrix first count)]
    (if (>= col-idx (dec num-cols))
      [(inc row-idx) 0]
      [row-idx (inc col-idx)])))

(defn- run-tachyon-simulation [diagram-matrix]
  (loop [row-idx 1
         col-idx 0
         num-splits 0
         updated-matrix diagram-matrix]
    (if (>= row-idx (count diagram-matrix))
      {:matrix updated-matrix
       :num-splits num-splits}
      (let [cell (get-in updated-matrix [row-idx col-idx])
            cell-above (get-in updated-matrix [(dec row-idx) col-idx])
            [next-row-idx next-col-idx] (get-next-cell row-idx col-idx updated-matrix)]
        (cond
          (and
           (= cell-above "|")
           (= cell "^")) (recur
                          next-row-idx
                          next-col-idx
                          (inc num-splits)
                          (-> updated-matrix
                              (assoc-in [row-idx (dec col-idx)] "|")
                              (assoc-in [row-idx (inc col-idx)] "|")))
          (or
           (= cell-above "S")
           (= cell-above "|")) (recur
                                next-row-idx
                                next-col-idx
                                num-splits
                                (assoc-in updated-matrix [row-idx col-idx] "|"))
          :else (recur
                 next-row-idx
                 next-col-idx
                 num-splits
                 updated-matrix))))))

(defn- matrix?
  [x]
  (and (sequential? x)
       (seq x)
       (sequential? (first x))
       (not (sequential? (first (first x))))))

(defn- vector-of-matrices?
  [x]
  (and (sequential? x)
       (seq x)
       (every? matrix? x)))

(defn- flatten-matrices
  [v]
  (reduce (fn [acc el]
            (cond
              (matrix? el) (conj acc el)
              (vector-of-matrices? el) (into acc el)
              :else (conj acc el)))
          []
          v))

(defn- create-new-timelines [row-idx col-idx timeline-diagram-matrix]
  (let [cell (get-in timeline-diagram-matrix [row-idx col-idx])
        cell-above (get-in timeline-diagram-matrix [(dec row-idx) col-idx])]
    (cond
      (and
       (= cell-above "|")
       (= cell "^")) [(assoc-in timeline-diagram-matrix [row-idx (dec col-idx)] "|")
                      (assoc-in timeline-diagram-matrix [row-idx (inc col-idx)] "|")]
      (or
       (= cell-above "S")
       (= cell-above "|")) [(assoc-in timeline-diagram-matrix [row-idx col-idx] "|")]
      :else timeline-diagram-matrix)))

(defn run-many-worlds-tachyon-simulation [diagram-matrix]
  (loop [row-idx 1
         col-idx 0
         timelines [diagram-matrix]]
    (if (>= row-idx (count diagram-matrix))
      (count timelines)
      (let [[next-row-idx next-col-idx] (get-next-cell row-idx col-idx diagram-matrix)
            new-timelines (->> timelines
                               (map (partial create-new-timelines row-idx col-idx))
                               (flatten-matrices))]
        (recur next-row-idx next-col-idx new-timelines)))))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment
  (def example-diagram [".......S......."
                        "..............."
                        ".......^......."
                        "..............."
                        "......^.^......"
                        "..............."
                        ".....^.^.^....."
                        "..............."
                        "....^.^...^...."
                        "..............."
                        "...^.^...^.^..."
                        "..............."
                        "..^...^.....^.."
                        "..............."
                        ".^.^.^.^.^...^."
                        "..............."])

  (diagram-rows->matrix example-diagram)

  (-> example-diagram
      diagram-rows->matrix
      run-tachyon-simulation))

;; For the solution...
(comment
  (-> "input/day07/input.txt"
      read-file-lines
      diagram-rows->matrix
      run-tachyon-simulation
      :num-splits))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment
  (def example-diagram [".......S......."
                        "..............."
                        ".......^......."
                        "..............."
                        "......^.^......"
                        "..............."
                        ".....^.^.^....."
                        "..............."
                        "....^.^...^...."
                        "..............."
                        "...^.^...^.^..."
                        "..............."
                        "..^...^.....^.."
                        "..............."
                        ".^.^.^.^.^...^."
                        "..............."])

  (-> example-diagram
      diagram-rows->matrix
      run-many-worlds-tachyon-simulation))
