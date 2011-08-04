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


