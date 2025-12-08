(ns aoc.problems.day08
  (:require [utils.files :refer [read-file-lines]]
            [clojure.set :refer [union]]
            [clojure.string :as string]))

(defn- straight-line-distance
  "https://en.wikipedia.org/wiki/Euclidean_distance"
  [p1 p2]
  (Math/sqrt
   (reduce + (map #(Math/pow (- %2 %1) 2) p1 p2))))

(defn- get-all-distances [junction-boxes]
  (->> (for [a junction-boxes
             b junction-boxes
             :when (not= a b)]
         [(sort [a b]) (straight-line-distance a b)])
       (sort-by second)
       dedupe))

(defn- connect-n-boxes [n junction-boxes]
  (let [mapped (get-all-distances junction-boxes)
        n-shortest (take n mapped)]
    (reduce
     (fn [circuits [[p1 p2] _]]
       (let [circuits-with-p1 (filter #(contains? % p1) circuits)
             circuits-with-p2 (filter #(contains? % p2) circuits)]
         (cond
           (and (nil? (first circuits-with-p1)) (nil? (first circuits-with-p2)))
           (conj circuits #{p1 p2})

           (and (first circuits-with-p1) (first circuits-with-p2))
           (conj (remove #(or (contains? % p1) (contains? % p2)) circuits)
                 (union (first circuits-with-p1) (first circuits-with-p2)))

           (first circuits-with-p1)
           (conj (remove #(contains? % p1) circuits)
                 (union (first circuits-with-p1) #{p1 p2}))

           (first circuits-with-p2)
           (conj (remove #(contains? % p2) circuits)
                 (union (first circuits-with-p2) #{p1 p2})))))

     []
     n-shortest)))

(defn- largest-circuits-product [junction-boxes num-connections]
  (->> junction-boxes
       (connect-n-boxes num-connections)
       (sort-by count >)
       (take 3)
       (map count)
       (reduce *)))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (def example-distances [[162 817 812]
                          [57 618 57]
                          [906 360 560]
                          [592 479 940]
                          [352 342 300]
                          [466 668 158]
                          [542 29 236]
                          [431 825 988]
                          [739 650 466]
                          [52 470 668]
                          [216 146 977]
                          [819 987 18]
                          [117 168 530]
                          [805 96 715]
                          [346 949 466]
                          [970 615 88]
                          [941 993 340]
                          [862 61 35]
                          [984 92 344]
                          [425 690 689]])

  (connect-n-boxes 10 example-distances)

  ;; 40
  (largest-circuits-product example-distances 10))

;; For the solution...
(comment
  (-> "input/day08/input.txt"
      read-file-lines
      (->> (mapv #(string/split % #","))
           (mapv #(mapv Long/parseLong %)))
      (largest-circuits-product 1000)))
