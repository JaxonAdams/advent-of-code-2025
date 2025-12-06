(ns aoc.problems.day06
  (:require [utils.files :refer [read-file-lines]]
            [clojure.string :as string]))

(defn- str->op [op-str]
  (let [operator-map {"+" +
                      "*" *}]
    (get operator-map op-str)))

(defn- parse-homework [homework-lines]
  (let [split-on-spaces (->> homework-lines
                             (map string/trim)
                             (map #(string/split % #"\s+")))
        operands (->> split-on-spaces
                      butlast
                      (map #(map Long/parseLong %)))
        operators (->> split-on-spaces
                       last
                       (map str->op))]
    (map-indexed
     (fn [idx operation]
       (into [operation]
             (map #(nth % idx) operands)))
     operators)))

(defn- solve-problem [problem]
  (apply (first problem) (rest problem)))

(defn solve-homework-problems [homework-lines]
  (->> homework-lines
       parse-homework
       (map solve-problem)
       (reduce +)))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment
  (def problems ["123 328  51 64 "
                 " 45 64  387 23 "
                 "  6 98  215 314"
                 "*   +   *   +  "])

  (parse-homework problems)

  ;; 33210
  (solve-problem [* 123 45 6])

  ;; 4277556
  (solve-homework-problems problems))

;; For the solution...
(comment
  (-> "input/day06/input.txt"
      read-file-lines
      solve-homework-problems))
