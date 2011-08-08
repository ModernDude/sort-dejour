(ns sort-dejour.core
  "Sorting Algorithms"
  (require [clojure.contrib.generic.comparison :as gcmp])
  (require [clojure.contrib.math :as math])
  (require [clojure.contrib.seq-utils :as sequ])
  (use [sort-dejour.util]))
 
 
(defn selection-sort [coll]
  "Sort a collection using the selection sort algorithm."
  {:algorithm "Selection Sort"
   :complexity
   {:best "O(n^2)"
    :average "O(n^2)"
    :worst "O(n^2)"}}
  (loop [coll coll acc []]
    (if (seq coll)
      (let [smallest (reduce gcmp/min coll)]
        (recur (remove-first #(= smallest %1) coll)
               (conj acc smallest)))
      acc)))


(defn bubble-sort [xs]
 "Sort a collection using the bubble sort algo. Todo: Accept Java Comparator..."
 {:algorithm "Bubble Sort"
  :complexity
  {:best "O(n)"
   :average "O(n^2)"
   :worst "O(n^2)"}}
 (letfn [(bubble [acc x]
           (if (seq acc)
             (if (> 0 (compare x (peek acc)))
               (conj (pop acc) x (peek acc))
               (conj acc x))
             [x]))]
   (loop [xs xs]
     (let [bubbled (reduce bubble [] xs)]
       (if (= xs bubbled)
         bubbled
         (recur bubbled))))))


(defn- insertion-sort-insert [coll a]
  (let [[fst scnd] (split-with #(>= 0 (compare %1 a)) coll)]
       (concat fst [a] scnd)))

(defn insertion-sort [coll]
  "Sort a collection using the insertion sort algo. Todo: Accept Java Comparator.."
  {:algorithm "Insertion Sort"
   :complexity
   {:best "O(n)"
    :average "O(n^2)"
    :worst "O(n^2)"}}
  (reduce insertion-sort-insert [] coll))


(defn- shell-sort-gaps [n]
  "Returns a sequence of the 'gaps' that are needed to compute
a shell short. n is the size of the coll to sort. This is nasty and needs
to be reworked."
  (let [start (reduce-until
               #(vector (inc (* 3 %2))) [] [1]
               #(> (first %2) n)
               (fn [a b] (first a)))]
    (take-while #(> %1 0) (iterate #(math/round (/ %1 3)) start ) )))

(defn shell-sort [coll]
  "Sort a collection using the shell sort algo. Todo: Accept Java Comparator.."
  {:algorithm "Shell Sort"
   :complexity
   {:best "O(n)"
    :average "O(nLog^2n)"
    :worst "O(nLog^2n)"}}
  (reduce #(mapcat
            insertion-sort
            (partition-by-nth %2 %1))
          coll
          (-> coll count shell-sort-gaps)))
    
(def *def-algo* bubble-sort )

(defn sort-algo
  "Sort the given collection using the provided algorithm. Add comparator."
  ([coll] (sort-algo coll *def-algo*))
  ([coll algo] (algo coll)))


(def algorithms [bubble-sort insertion-sort selection-sort])
