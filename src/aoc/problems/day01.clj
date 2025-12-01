(ns aoc.problems.day01
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

(defn- parse-instruction [instruction]
  (let [op (cond
             (-> instruction first (= \R)) +
             (-> instruction first (= \L)) -
             :else (throw (Exception. (str "Invalid instruction provided: " instruction))))
        num-clicks (->> instruction rest (string/join #"") Integer/parseInt)]
    [op num-clicks]))

(defn- turn-dial [rotations current-pos]
  (when (seq rotations)
    (let [[turn-fn num-clicks] (-> rotations first parse-instruction)]
      [(rest rotations) (mod (turn-fn current-pos num-clicks) 100)])))

;; From the example: 0-99 nums; "-" => "L", "+" => "R"
(comment
  (mod (- 50 68) 100)
  (mod (- 82 30) 100)
  (mod (+ 52 48) 100))

(comment
  (read-file-lines "input/day01/problem-1-input.txt"))

(comment (parse-instruction "L1"))

(comment (turn-dial ["L68" "L30"] 50))
