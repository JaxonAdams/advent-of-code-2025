(ns aoc.problems.day10
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [utils.files :refer [read-file-lines]]))

;; TODO: clean up this code!

(defn- parse-joltage-diagram [diagram-str]
  (-> diagram-str
      rest
      butlast
      (->> (apply str))
      (string/split #",")
      (->> (map Integer/parseInt))))

(defn- parse-indicator-light-diagram [diagram-str]
  (->> diagram-str
       rest
       butlast
       (map
        (fn [c]
          (cond
            (= \. c) false
            (= \# c) true)))))

(defn- parse-button-wiring-schematics [buttons]
  (map #(->> (re-seq #"\d+" %)
             (map Integer/parseInt)) buttons))

(defn- parse-instruction [instruction]
  (let [parts (string/split instruction #" ")
        indicator-light-diagram (parse-indicator-light-diagram (first parts))
        button-wiring-schematics (parse-button-wiring-schematics (-> parts rest butlast))
        joltage-diagram (parse-joltage-diagram (last parts))]
    {:light-diagram indicator-light-diagram
     :button-schematics button-wiring-schematics
     :joltage-diagram joltage-diagram}))

(defn- button-press-combinations [buttons n]
  (let [with-n-repeats (mapcat (partial repeat n) buttons)]
    (->> with-n-repeats
         count
         inc
         (range 1)
         (mapcat #(combo/combinations with-n-repeats %)))))

(defn- press-buttons [initial-state buttons update-fn]
  (reduce (fn [new-state diagram-idx]
            (->> (nth new-state diagram-idx)
                 update-fn
                 (assoc new-state diagram-idx)))
          (vec initial-state)
          (flatten buttons)))

(defn- min-btn-presses-for-lights [desired-state-diagram buttons n-combos update-fn initial-value]
  (let [initial-state (repeat (count desired-state-diagram) initial-value)
        button-combos (button-press-combinations buttons n-combos)]
    (->> button-combos
         (filter #(= desired-state-diagram (press-buttons initial-state % update-fn)))
         (sort-by count)
         first
         count)))

(defn- fewest-presses-required-light-config [instruction-strs]
  (let [instructions (map parse-instruction instruction-strs)]
    (->> instructions
         (map (fn [{:keys [light-diagram button-schematics]}]
                (min-btn-presses-for-lights light-diagram button-schematics 1 not false)))
         (reduce +))))

(defn press-vector
  [dim button]
  (reduce (fn [v idx] (update v idx inc))
          (vec (repeat dim 0))
          button))

(defn max-presses
  [target effect]
  (let [quotients (keep-indexed
                   (fn [i e]
                     (when (pos? e)
                       (quot (nth target i) e)))
                   effect)]
    (if (seq quotients)
      (apply min quotients)
      0)))

(defn- update-transient-map
  [t-map new-vec new-cost]
  (let [old-cost (clojure.core/get t-map new-vec)]
    (if old-cost
      (if (< new-cost old-cost)
        (assoc! t-map new-vec new-cost)
        t-map)
      (assoc! t-map new-vec new-cost))))

(defn enumerate-half
  [target btn-effects]
  (let [n (count btn-effects)
        D (count target)]
    (letfn [(recur-gen [i]
              (if (= i n)
                {(vec (repeat D 0)) 0}
                (let [eff (nth btn-effects i)
                      m (max-presses target eff)
                      rest-map (recur-gen (inc i))
                      t-map (transient rest-map)]

                  (loop [k 1
                         current-t-map t-map]
                    (if (> k m)
                      (persistent! current-t-map)

                      (let [delta (mapv #(* k %) eff)]
                        (let [new-map
                              (reduce
                               (fn [acc-t-map [vec cost]]
                                 (let [new-vec (mapv + vec delta)
                                       new-cost (+ cost k)]
                                   (if (every? (fn [[a b]] (<= a b)) (map vector new-vec target))
                                     (update-transient-map acc-t-map new-vec new-cost)
                                     acc-t-map)))
                               current-t-map
                               rest-map)]
                          (recur (inc k) new-map))))))))]
      (recur-gen 0))))

(defn combine-maps
  [map1 map2 target]
  (let [combinations
        (for [[v1 c1] map1
              [v2 c2] map2
              :let [new-v (mapv + v1 v2)
                    new-c (+ c1 c2)]
              :when (every? (fn [[a b]] (<= a b)) (map vector new-v target))]
          [new-v new-c])]
    (reduce
     (fn [acc [vec cost]]
       (update acc vec (fn [old-cost]
                         (if old-cost
                           (min old-cost cost)
                           cost))))
     {}
     combinations)))

(defn min-btn-presses
  [target buttons]
  (let [D (count target)
        effects (mapv (partial press-vector D) buttons)
        n (count effects)
        q (quot n 4)

        effects-a (subvec effects 0 q)
        effects-b (subvec effects q (* 2 q))
        effects-c (subvec effects (* 2 q) (* 3 q))
        effects-d (subvec effects (* 3 q) n)

        map-a (enumerate-half target effects-a)
        map-b (enumerate-half target effects-b)
        map-c (enumerate-half target effects-c)
        map-d (enumerate-half target effects-d)

        map-ab (combine-maps map-a map-b target)

        map-cd (combine-maps map-c map-d target)

        final-result
        (reduce
         (fn [best [v-ab c-ab]]
           (let [need (mapv - target v-ab)
                 c-cd (get map-cd need)]
             (cond
               (nil? c-cd) best
               :else (let [total-cost (+ c-ab c-cd)]
                       (if (or (nil? best) (< total-cost best))
                         total-cost
                         best)))))
         nil
         map-ab)]

    final-result))

(defn- fewest-presses-required-joltage-config [instruction-strs]
  (let [instructions (map parse-instruction instruction-strs)]
    (->> instructions
         (map (fn [{:keys [joltage-diagram button-schematics]}]
                (prn "Finding presses for:" joltage-diagram)
                (min-btn-presses joltage-diagram button-schematics)))
         (reduce +))))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (parse-instruction "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

  (press-buttons [false true true false] [[1 3] [3]] not)

  (press-buttons [false false false false false] [[0 4] [0 1 2] [1 2 3 4]] not)

  (button-press-combinations [[3] [1 3] [2] [2 3] [0 2] [0 1]] 1)

  ;; 2
  (min-btn-presses-for-lights [false true true false] [[3] [1 3] [2] [2 3] [0 2] [0 1]] 1 not false)
  ;; 3
  (min-btn-presses-for-lights [false false false true false] [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] 1 not false)
  ;; 2
  (min-btn-presses-for-lights [false true true true false true] [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] 1 not false)

  (def example-instructions ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                             "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                             "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"])

  ;; 7
  (fewest-presses-required-light-config example-instructions))

;; For the solution...
(comment
  (-> "input/day10/input.txt"
      read-file-lines
      fewest-presses-required-light-config))

;; ----------------------------------------------------------------------------
;; PART TWO

(comment

  ;; 10
  (min-btn-presses [3 5 4 7] [[3] [1 3] [2] [2 3] [0 2] [0 1]])

  ;; 33
  (fewest-presses-required-joltage-config example-instructions)

  (fewest-presses-required-joltage-config
   ["[####.###.#] (0,1,2,8,9) (0,2,9) (0,1,4,5,9) (0,3,7,9) (1,4,5) (1,2,8,9) (0,3,5,6,7,8) (0,2,5,7,9) (4,5,6) (2,3,4,7,8) {177,39,175,23,41,176,22,157,28,186}"]))

;; For the solution...
(comment
  (-> "input/day10/input.txt"
      read-file-lines
      fewest-presses-required-joltage-config))
