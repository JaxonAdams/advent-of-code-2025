(ns aoc.problems.day06
  (:require [utils.files :refer [read-file-lines]]
            [clojure.string :as string]))

(defn- str->op [op-str]
  (let [operator-map {"+" +
                      "*" *}]
    (->> op-str
         string/trim
         (get operator-map))))

(defn- pad-str [s max-len pad-char]
  (str (apply str (repeat (- max-len (count s)) pad-char)) s))

(defn- preprocess-operands [operands cephalopod-style?]
  (if cephalopod-style?
    (map #(pad-str % (->> operands (map count) (apply max)) " ") operands)
    (->> operands
         (map string/trim)
         (map Long/parseLong))))

(defn- split-string-at-indices [s indices]
  (let [sorted-indices (sort (distinct indices))
        split-points (conj (vec sorted-indices) (count s))
        start-ends (partition 2 1 (cons 0 split-points))]
    (mapv (fn [[start end]] (subs s start end)) start-ends)))

(defn- transpose [rows]
  (apply mapv (fn [& chars] (apply str chars)) rows))

(defn- parse-homework [homework-lines cephalopod-style?]
  (let [blanks-subbed (->> homework-lines
                           (map #(string/replace % #" " "X")))
        split-indices (->> blanks-subbed
                           first
                           (map-indexed
                            (fn [idx _]
                              (and (apply = (map #(nth % idx) blanks-subbed))
                                   idx)))
                           (filter identity))
        split-by-columns (->> homework-lines
                              (map #(split-string-at-indices % split-indices)))
        operands (->> split-by-columns
                      butlast
                      (map #(preprocess-operands % cephalopod-style?)))
        operators (->> split-by-columns
                       last
                       (map str->op))
        grouped (map-indexed
                 (fn [idx operation]
                   (into [operation]
                         (map #(nth % idx) operands)))
                 operators)]
    (if cephalopod-style?
      (->> grouped
           (map rest)
           (map #(map (fn [s] (apply str (rest s))) %))
           (map transpose)
           (map #(map (fn [s] (Long/parseLong (string/trim s))) %))
           (map (fn [op operands] (into [op] operands)) operators))
      grouped)))

(defn- solve-problem [problem]
  (apply (first problem) (rest problem)))

(defn solve-homework-problems [homework-lines cephalopod-style?]
  (-> homework-lines
      (parse-homework cephalopod-style?)
      (->> (map solve-problem)
           (reduce +))))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment
  (def problems ["123 328  51 64 "
                 " 45 64  387 23 "
                 "  6 98  215 314"
                 "*   +   *   +  "])

  (parse-homework problems false)

  ;; 33210
  (solve-problem [* 123 45 6])

  ;; 4277556
  (solve-homework-problems problems false))

;; For the solution...
(comment
  (-> "input/day06/input.txt"
      read-file-lines
      (solve-homework-problems false)))

;; ----------------------------------------------------------------------------
;; PART TWO

(comment
  (def problems ["123 328  51 64 "
                 " 45 64  387 23 "
                 "  6 98  215 314"
                 "*   +   *   +  "])

  (parse-homework problems true)

  ;; 3263827
  (solve-homework-problems problems true))

;; For the solution...
(comment
  (-> "input/day06/input.txt"
      read-file-lines
      (solve-homework-problems true)))
