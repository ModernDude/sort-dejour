(ns sort-dejour.core
  "Sorting Algorithms")

  
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

(defn insertion-sort [coll]
  "Sort a collection using the insertion sort algo. Todo: Accept Java Comparator.."
  {:algorithm "Insertion Sort"
   :complexity
   {:best "O(n)"
    :average "O(n^2)"
    :worst "O(n^2)"}}
  (letfn [(ins-insert [coll a]
           (let [[f s] (split-with #(>= 0 (compare %1 a)) coll)]
             (concat f [a] s)))]
    (reduce ins-insert [] coll)))


(def *def-algo* bubble-sort )


(defn sort-algo
  "Sort the given collection using the provided algorithm. Add comparator."
  ([coll] (sort-algo coll *def-algo*))
  ([coll algo] (algo coll)))


(def algorithms [bubble-sort insertion-sort])

         

