(ns aoc.problems.day02-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc.problems.day02 :as solution]))

(deftest part-one-examples
  (testing "Finds invalid ids in range 11-22"
    (is (= (solution/get-invalid-ids solution/is-twice-repeated-sequence? "11-22") '(11 22))
        "Should identify 11 and 22 correctly"))
  (testing "Finds invalid ids in range 95-115"
    (is (= (solution/get-invalid-ids solution/is-twice-repeated-sequence? "95-115") '(99))
        "Should identify 99 correctly"))
  (testing "Finds invalid ids in range 998-1012"
    (is (= (solution/get-invalid-ids solution/is-twice-repeated-sequence? "998-1012") '(1010))
        "Should identify 1010 correctly"))
  (testing "Finds invalid ids in range 1188511880-1188511890"
    (is (= (solution/get-invalid-ids solution/is-twice-repeated-sequence? "1188511880-1188511890") '(1188511885))
        "Should identify 1188511885 correctly"))
  (testing "Finds the correct sum with the given ranges"
    (is (=
         (solution/get-invalid-id-sum
          solution/is-twice-repeated-sequence?
          ["11-22"
           "95-115"
           "998-1012"
           "1188511880-1188511890"
           "222220-222224"
           "1698522-1698528"
           "446443-446449"
           "38592856-38593862"
           "565653-565659"
           "824824821-824824827"
           "2121212118-2121212124"])
         1227775554)
        "Should correctly identify the sum")))

(deftest part-two-examples
  (testing "Finds invalid ids in range 11-22"
    (is (= (solution/get-invalid-ids solution/is-repeated-sequence? "11-22") '(11 22))
        "Should identify 11 and 22 correctly"))
  (testing "Finds invalid ids in range 95-115"
    (is (= (solution/get-invalid-ids solution/is-repeated-sequence? "95-115") '(99 111))
        "Should identify 99 and 111 correctly"))
  (testing "Finds invalid ids in range 998-1012"
    (is (= (solution/get-invalid-ids solution/is-repeated-sequence? "998-1012") '(999 1010))
        "Should identify 999 and 1010 correctly"))
  (testing "Finds invalid ids in range 1188511880-1188511890"
    (is (= (solution/get-invalid-ids solution/is-repeated-sequence? "1188511880-1188511890") '(1188511885))
        "Should identify 1188511885 correctly"))
  (testing "Finds the correct sum with the given ranges"
    (is (=
         (solution/get-invalid-id-sum
          solution/is-repeated-sequence?
          ["11-22"
           "95-115"
           "998-1012"
           "1188511880-1188511890"
           "222220-222224"
           "1698522-1698528"
           "446443-446449"
           "38592856-38593862"
           "565653-565659"
           "824824821-824824827"
           "2121212118-2121212124"])
         4174379265)
        "Should correctly identify the sum")))
