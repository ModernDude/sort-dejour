(ns sort-dejour.test.util
  (:use [sort-dejour.util])
  (:use [clojure.test]))


(deftest test-remove-first
  (testing "Remove One Value From a collection"
    (is (= [] (remove-first #(= 1 %1) [1]))))
  (testing "Dup values in list =, only one removed"
    (is (= [1] (remove-first #(= 1 %1) [1 1])))
    (is (= [1 3] (remove-first #(= 1 %1) [1 1 3])))
    (is (= [3 1] (remove-first #(= 1 %1) [3 1 1])))))


(deftest test-partition-by-nth
  (testing "Nil in Nil Out"
    (is (= nil (partition-by-nth  1 nil))))
  (testing "1 n set yeield input list"
    (is (= [[1 2 3 4]] (partition-by-nth 1 [1 2 3 4]))))
  (testing "2 n yelds..."
    (is (= [[1 3][2 4]] (partition-by-nth 2 [1 2 3 4 ]))))
  (testing "No even number in out put lists"
    (is (= [[1 4][2][3]] (partition-by-nth 3 [1 2 3 4])))) 
  (testing "Partition by more than in coll"
    (is (= [[1][2][3][4][]] (partition-by-nth 5 [1 2 3 4])))))
