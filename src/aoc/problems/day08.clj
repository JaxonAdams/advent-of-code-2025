(ns aoc.problems.day08
  (:require [aoc.utils.files :refer [read-file-lines]]
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

;; Similar to 'get-all-distances', but returning a different structure
(defn- all-edges
  [points]
  (let [pts (vec points)
        n (count pts)]
    (into []
          (for [i (range n)
                j (range (inc i) n)]
            {:u i :v j :w (straight-line-distance (pts i) (pts j))}))))

;; Union-Find dataset for usage in Kruskal's algorithm
(defn- uf-find
  [parents x]
  (letfn [(find-path [p i stack]
            (let [parent (p i)]
              (if (= parent i)
                [i stack]
                (recur p parent (conj stack i)))))]
    (let [[root path] (find-path parents x [])]
      (if (seq path)
        [root (reduce (fn [acc idx] (assoc acc idx root)) parents path)]
        [root parents]))))

(defn- uf-union
  [parents ranks x y]
  (let [[rx parents'] (uf-find parents x)
        [ry parents''] (uf-find parents' y)]
    (if (= rx ry)
      [parents'' ranks false]
      (let [rrx (ranks rx)
            rry (ranks ry)]
        (cond
          (< rrx rry) [(assoc parents'' rx ry) ranks true]
          (> rrx rry) [(assoc parents'' ry rx) ranks true]
          :else [(assoc parents'' ry rx) (assoc ranks rx (inc rrx)) true])))))

;; Kruskal algorithm for finding MST
(defn- kruskal-mst
  [points]
  (let [pts (vec points)
        n (count pts)]
    (if (<= n 1)
      {:mst-edges [] :total-weight 0.0}
      (let [edges (->> (all-edges pts) (sort-by :w))
            parents (vec (range n))
            ranks (vec (repeat n 0))]
        (loop [remaining edges
               parents parents
               ranks ranks
               mst []
               total 0.0]
          (if (or (empty? remaining) (= (count mst) (dec n)))
            {:mst-edges mst :total-weight total}
            (let [{:keys [u v w] :as e} (first remaining)
                  [parents' ranks' did-union] (uf-union parents ranks u v)]
              (recur (rest remaining)
                     parents'
                     ranks'
                     (if did-union
                       (conj mst (assoc e :pu (nth pts u) :pv (nth pts v)))
                       mst)
                     (if did-union (+ total w) total)))))))))

(defn- final-boxes-x-product [junction-boxes]
  (-> junction-boxes
      kruskal-mst
      :mst-edges
      last
      (select-keys [:pu :pv])
      vals
      (->> (map first)
           (reduce *))))

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

;; ----------------------------------------------------------------------------
;; PART TWO

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

  (def test-pts [[0 0 0] [0 0 1] [0 1 0] [1 0 0]])
  (kruskal-mst test-pts)

  (kruskal-mst example-distances)

  ;; 25272
  (final-boxes-x-product example-distances))

;; For the solution...
(comment
  (-> "input/day08/input.txt"
      read-file-lines
      (->> (mapv #(string/split % #","))
           (mapv #(mapv Long/parseLong %)))
      final-boxes-x-product))
