(ns aoc.problems.day05-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc.problems.day05 :as solution]))

(def ^:private fresh-ranges ["3-5"
                             "10-14"
                             "16-20"
                             "12-18"])

(deftest utility-functions

  (testing "Correctly identifies if an ingredient is in a given range."
    (is (true? (solution/ingredient-is-in-range 4 "3-5"))
        "Should identify that 4 is in range 3-5")
    (is (true? (solution/ingredient-is-in-range 5 "3-5"))
        "The range should be inclusive")
    (is (false? (solution/ingredient-is-in-range 2 "3-5"))
        "Should correctly identify ingredients outside the given range."))

  (testing "Correctly identifies if an ingredient is fresh."
    (is (false? (solution/is-fresh? 1 fresh-ranges)))
    (is (true? (solution/is-fresh? 5 fresh-ranges)))
    (is (false? (solution/is-fresh? 8 fresh-ranges)))
    (is (true? (solution/is-fresh? 11 fresh-ranges)))
    (is (false? (solution/is-fresh? 32 fresh-ranges)))))

(deftest part-one
  (testing "Correctly identifies the total number of fresh ingredients as 3."
    (is (= 3 (solution/total-fresh-from-available ["1" "5" "8" "11" "17" "32"] fresh-ranges)))))

(deftest part-two
  (testing "Correctly identifies the total number of fresh ingredients as 14."
    (is (= 14 (solution/total-fresh fresh-ranges)))))
