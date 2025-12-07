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

(defn next-cells
  [[row col] diagram]
  (let [cell (get-in diagram [row col])
        cell-above (get-in diagram [(dec row) col])]
    (cond
      (and (= cell "^") (= [(dec row) col] [(dec row) col]))
      #{[(inc row) (dec col)]
        [(inc row) (inc col)]}

      (or (= cell-above "S")
          (= [(dec row) col] [(dec row) col]))
      #{[(inc row) col]}

      :else
      #{})))

(defn count-tachyon-paths
  [diagram [start-row start-col]]
  (let [rows (count diagram)
        memo (atom {})]

    (declare count-from)

    (defn count-from [[row col]]
      (cond
        (>= row rows) 1
        (@memo [row col]) (@memo [row col])
        :else
        (let [nexts (next-cells [row col] diagram)
              cnt   (reduce + (map count-from nexts))]
          (swap! memo assoc [row col] cnt)
          cnt)))

    (count-from [start-row start-col])))

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
      (count-tachyon-paths [1 7])))

;; For the solution...
(comment
  (-> "input/day07/input.txt"
      read-file-lines
      diagram-rows->matrix
      (count-tachyon-paths [1 70])))
