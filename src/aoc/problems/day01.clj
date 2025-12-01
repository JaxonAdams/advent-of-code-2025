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

(defn- turn-dial [current-pos instruction]
  (let [[turn-fn num-clicks] (parse-instruction instruction)]
    (-> current-pos
        (turn-fn num-clicks)
        (mod 100))))

(defn- get-password [rotations]
  (reduce turn-dial 50 rotations))

;; AOC example:
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

