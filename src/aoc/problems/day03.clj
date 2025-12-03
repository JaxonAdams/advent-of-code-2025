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

(defn- max-voltage [battery-bank]
  (let [voltages (-> battery-bank
                     (string/split #"")
                     (->> (map Integer/parseInt)
                          (map-indexed vector)))
        tens-candidates (reverse (sort-by second (take (dec (count battery-bank)) voltages)))
        max-voltages-tens (sort-by first (filter #(-> tens-candidates first second (= (second %))) tens-candidates))
        max-voltage (first max-voltages-tens)
        ones-candidates (reverse (sort-by second (second (split-at (-> max-voltage first inc) voltages))))
        next-max-voltage (first ones-candidates)]
    (->> [max-voltage next-max-voltage]
         (map second)
         (apply str)
         Integer/parseInt)))

(defn- max-voltage-twelve-batteries [battery-bank]
  (let [v (parse-digits-with-idx battery-bank)
        n (count v)
        tens-slice (subvec v 0 (- n 11))
        max-digit (apply max (map second tens-slice))
        first-pick (first (filter #(= (second %) max-digit) tens-slice))
        first-idx  (first first-pick)
        rest-picks (largest-subseq-from v (inc first-idx) 11)]
    (->> (cons first-pick rest-picks)
         (map second)
         (apply str)
         Long/parseLong)))

(defn sum-of-max-volts [battery-banks]
  (->> battery-banks
       (map max-voltage)
       (reduce +)))

(defn sum-of-max-volts-twelve-batteries [battery-banks]
  (->> battery-banks
       (map max-voltage-twelve-batteries)
       (reduce +)))

;; ----------------------------------------------------------------------------
;; PART 1

(comment
  ;; 98
  (max-voltage "987654321111111")
  ;; 89
  (max-voltage "811111111111119")
  ;; 78
  (max-voltage "234234234234278")
  ;; 92
  (max-voltage "818181911112111"))

;; EDGE CASES
(comment
  ;; 99
  (max-voltage "997654321111111"))

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
