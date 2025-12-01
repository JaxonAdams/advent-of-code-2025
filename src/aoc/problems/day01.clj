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

(defn- turn-dial [pos-info instruction]
  (let [[current-pos num-zeros] pos-info
        [turn-fn num-clicks] (parse-instruction instruction)
        new-pos (-> current-pos
                    (turn-fn num-clicks)
                    (mod 100))]
    [new-pos (if (zero? new-pos) (inc num-zeros) num-zeros)]))

(defn- execute-instructions [rotations]
  (reduce turn-dial [50 0] rotations))

(defn get-password [rotations]
  (-> rotations
      execute-instructions
      second))

;; AOC example (should equal 3):
(comment
  (get-password ["L68"
                 "L30"
                 "R48"
                 "L5"
                 "R60"
                 "L55"
                 "L1"
                 "L99"
                 "R14"
                 "L82"]))

(comment
  (-> "input/day01/problem-1-input.txt"
      read-file-lines
      get-password))

