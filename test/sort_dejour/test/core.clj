(ns sort-dejour.test.core
  (:use [sort-dejour.core])
  (:use [clojure.test]))

(deftest test-all
  (doseq [algo algorithms]
    (testing "Sorting empty coll results in empty coll"
      (is (= [] (sort-algo [] algo))))
    (testing "Pre Sorted list results in itself"
      (let [pre-sorted (list 1 2 3 4)]
        (is (= pre-sorted (sort-algo pre-sorted algo)))))
    (testing "Pre sorted vector results in iteslf"
      (let [pre-sorted [1 2 3 4]]
        (is (= pre-sorted (sort-algo pre-sorted algo)))))
    (testing "Reversed coll gets sorted"
      (is (= [1 2 3 4 5 6] (sort-algo [6 5 4 3 2 1] algo))))
    (testing "Mixed coll gets sorted"
      (is (= [-1 0 1 2 2 3 8 9 99] (sort-algo [1 9 0 -1 2 99 3 2 8] algo))))
    ))


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

