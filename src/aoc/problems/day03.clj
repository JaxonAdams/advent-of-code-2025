(ns aoc.problems.day03
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

(defn- parse-digits-with-idx
  [s]
  (vec
   (keep-indexed
    (fn [idx char]
      [idx (Character/digit char 10)])
    s)))

(defn- find-max-pick
  [candidates]
  (let [max-val (apply max (map second candidates))]
    (first (filter #(= max-val (second %)) candidates))))

(defn- largest-subseq-from
  [v start k]
  (when (pos? k)
    (let [n (count v)
          last-idx (- n k)
          candidates (subvec v start (inc last-idx))
          pick (find-max-pick candidates)
          p-idx (first pick)]
      (cons pick (largest-subseq-from v (inc p-idx) (dec k))))))

(defn- get-max-voltage
  [battery-bank num-batteries-to-use]
  (let [v (parse-digits-with-idx battery-bank)
        picks (largest-subseq-from v 0 num-batteries-to-use)]
    (->> picks
         (map second)
         (apply str)
         Long/parseLong)))

(defn- max-voltage-two-batteries [battery-bank]
  (get-max-voltage battery-bank 2))

(defn- max-voltage-twelve-batteries [battery-bank]
  (get-max-voltage battery-bank 12))

(defn sum-of-max-volts [battery-banks]
  (->> battery-banks
       (map max-voltage-two-batteries)
       (reduce +)))

(defn sum-of-max-volts-twelve-batteries [battery-banks]
  (->> battery-banks
       (map max-voltage-twelve-batteries)
       (reduce +)))

;; ----------------------------------------------------------------------------
;; PART 1

(comment
  ;; 98
  (max-voltage-two-batteries "987654321111111")
  ;; 89
  (max-voltage-two-batteries "811111111111119")
  ;; 78
  (max-voltage-two-batteries "234234234234278")
  ;; 92
  (max-voltage-two-batteries "818181911112111"))

;; EDGE CASES
(comment
  ;; 99
  (max-voltage-two-batteries "997654321111111"))

(comment
  ;; 357
  (sum-of-max-volts ["987654321111111"
                     "811111111111119"
                     "234234234234278"
                     "818181911112111"]))

;; Part 1 Solution...
(comment
  (-> "input/day03/input.txt"
      read-file-lines
      sum-of-max-volts))

;; ----------------------------------------------------------------------------
;; PART 2

(comment
  ;; 987654321111
  (max-voltage-twelve-batteries "987654321111111")
  ;; 811111111119
  (max-voltage-twelve-batteries "811111111111119")
  ;; 434234234278
  (max-voltage-twelve-batteries "234234234234278")
  ;; 888911112111
  (max-voltage-twelve-batteries "818181911112111"))

;; EDGE CASES
(comment
  ;; 999999876549
  (max-voltage-twelve-batteries "999999876543219"))

(comment
  ;; 3121910778619
  (sum-of-max-volts-twelve-batteries ["987654321111111"
                                      "811111111111119"
                                      "234234234234278"
                                      "818181911112111"]))

;; Part 2 solution...
(comment
  (-> "input/day03/input.txt"
      read-file-lines
      sum-of-max-volts-twelve-batteries))
