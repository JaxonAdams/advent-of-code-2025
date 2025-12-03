(ns aoc.problems.day03
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

;; TODO: Come back and clean this up!
;; I got the right answer but this code could use some work...

(defn- parse-digits-with-idx
  [s]
  (->> (string/split s #"")
       (map #(Integer/parseInt %))
       (map-indexed vector)
       vec))

(defn largest-subseq-from
  [v start k]
  (let [n (count v)]
    (loop [i start, remaining k, chosen []]
      (if (zero? remaining)
        chosen
        (let [last-idx (- n remaining)
              candidates (subvec v i (inc last-idx))
              max-digit (apply max (map second candidates))
              pick     (first (filter #(= max-digit (second %)) candidates))
              p-idx    (first pick)]
          (recur (inc p-idx) (dec remaining) (conj chosen pick)))))))

(defn- get-max-voltage [battery-bank num-batteries-to-use]
  (let [offset (dec num-batteries-to-use)
        v (parse-digits-with-idx battery-bank)
        n (count v)
        tens-slice (subvec v 0 (- n offset))
        max-digit (apply max (map second tens-slice))
        first-pick (first (filter #(= (second %) max-digit) tens-slice))
        first-idx  (first first-pick)
        rest-picks (largest-subseq-from v (inc first-idx) offset)]
    (->> (cons first-pick rest-picks)
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
