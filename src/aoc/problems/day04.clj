(ns aoc.problems.day04
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

(defn- get-neighbors-in-bounds [[x y] diagram-rows]
  (let [rows (count diagram-rows)
        cols (count (first diagram-rows))]
    (for [dx [-1 0 1]
          dy [-1 0 1]
          :let [nx (+ x dx)
                ny (+ y dy)]
          :when (and
                 (not (and (zero? dx) (zero? dy)))
                 (<= 0 nx (dec rows))
                 (<= 0 ny (dec cols)))]
      [nx ny])))

(defn- get-character-from-pos [diagram-rows [x y]]
  (-> diagram-rows (nth x) (nth y) str))

(defn- is-roll-accessible? [pos diagram-rows]
  (if (-> diagram-rows (get-character-from-pos pos) (not= "@"))
    false
    (let [adjacent-positions (get-neighbors-in-bounds pos diagram-rows)
          adjacent-chars (map (partial get-character-from-pos diagram-rows) adjacent-positions)
          counted (reduce (fn [counts item]
                            (assoc counts item (inc (get counts item 0))))
                          {}
                          adjacent-chars)]
      (-> counted (get "@" 0) (< 4)))))

(defn get-num-of-accessible-rolls [diagram-rows]
  (let [diagram-matrix (map #(string/split % #"") diagram-rows)
        accessible-matrix (map
                           (fn [row-idx]
                             (map
                              (fn [col-idx]
                                (is-roll-accessible? [row-idx col-idx] diagram-matrix))
                              (range 0 (count (nth diagram-matrix row-idx)))))
                           (range 0 (count diagram-rows)))]
    (->> accessible-matrix
         flatten
         (filter identity)
         count)))

(defn total-removable-rolls [diagram-rows]
  (let [grid (mapv #(vec (string/split % #"")) diagram-rows)
        rows (count grid)
        cols (count (first grid))]
    (loop [g grid
           total-removed 0]
      ;; Find all currently accessible rolls
      (let [accessible (for [r (range rows)
                             c (range cols)
                             :when (= "@" (get-in g [r c]))
                             :when (is-roll-accessible? [r c] g)]
                         [r c])]
        (if (empty? accessible)
          total-removed
          ;; Remove all accessible rolls this round and continue
          (let [g' (reduce (fn [gr [r c]] (assoc-in gr [r c] ".")) g accessible)]
            (recur g' (+ total-removed (count accessible)))))))))

;; ----------------------------------------------------------------------------
;; PART ONE

;; '@' = roll of paper
(comment
  (def example-diagram ["..@@.@@@@."
                        "@@@.@.@.@@"
                        "@@@@@.@.@@"
                        "@.@@@@..@."
                        "@@.@@@@.@@"
                        ".@@@@@@@.@"
                        ".@.@.@.@@@"
                        "@.@@@.@@@@"
                        ".@@@@@@@@."
                        "@.@.@@@.@."]) ;; 13 can be accessed
  )

(comment
  (def example-diagram-matrix (map #(string/split % #"") example-diagram)))

(comment
  ;; false
  (is-roll-accessible? [0 0] example-diagram-matrix)
  ;; false
  (is-roll-accessible? [0 7] example-diagram-matrix)
  ;; true
  (is-roll-accessible? [2 6] example-diagram-matrix)
  ;; true
  (is-roll-accessible? [9 0] example-diagram-matrix)
  ;; true
  (is-roll-accessible? [4 9] example-diagram-matrix)
  ;; false
  (is-roll-accessible? [5 9] example-diagram-matrix))

(comment
  ;; 13
  (get-num-of-accessible-rolls example-diagram))

;; For the solution...
(comment
  (-> "input/day04/input.txt"
      read-file-lines
      (->> (map (partial string/join)))
      get-num-of-accessible-rolls))

;; ----------------------------------------------------------------------------
;; PART TWO

(comment
  ;; 43
  (def example-diagram ["..@@.@@@@."
                        "@@@.@.@.@@"
                        "@@@@@.@.@@"
                        "@.@@@@..@."
                        "@@.@@@@.@@"
                        ".@@@@@@@.@"
                        ".@.@.@.@@@"
                        "@.@@@.@@@@"
                        ".@@@@@@@@."
                        "@.@.@@@.@."])

  (total-removable-rolls example-diagram))

;; For the solution...
(comment
  (-> "input/day04/input.txt"
      read-file-lines
      (->> (map (partial string/join)))
      total-removable-rolls))
