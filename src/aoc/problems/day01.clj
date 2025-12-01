(ns aoc.problems.day01
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

(defn- floor-of-div-100 [n]
  (-> n
      (/ 100.0)
      Math/floor
      int))

(defn- count-zero-crossings [current-pos turn-fn num-clicks]
  (let [raw-new-pos ((if (= turn-fn +) + -) current-pos num-clicks)]
    (if (= turn-fn +)
      (let [p-start-adjusted (if (zero? current-pos) 100 current-pos)]
        (- (floor-of-div-100 raw-new-pos)
           (floor-of-div-100 (dec p-start-adjusted))))
      (- (floor-of-div-100 (dec current-pos))
         (floor-of-div-100 (dec raw-new-pos))))))

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
        raw-new-pos (turn-fn current-pos num-clicks)

        new-pos (mod (mod raw-new-pos 100) 100)

        times-passed-zero (count-zero-crossings current-pos turn-fn num-clicks)]

    [new-pos
     (+ num-zeros times-passed-zero)]))

(defn- execute-instructions [rotations]
  (reduce turn-dial [50 0] rotations))

(defn get-password [rotations]
  (-> rotations
      execute-instructions
      second))

;; AOC example (should equal 6):
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
  (get-password ["R1000"]))

(comment
  (-> "input/day01/problem-1-input.txt"
      read-file-lines
      get-password))

