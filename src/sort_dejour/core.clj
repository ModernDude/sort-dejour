(ns sort-dejour.core
  "Sorting Algorithms")


(def *def-algo* bubble-sort)


(defn sort-algo
  "Sort the given collection using the provided algorithm. Add comparator."
  ([coll] (sort-algo coll *def-algo*))
  ([coll algo] (algo coll)))
  
 
(defn bubble-sort [xs]
  "Sort a collection using the bubble sort algo. Todo: Accept Java Comparator...
This seems way to verbose and I feel like this could be reworked. I might try another approach using a reduce since it looks at two items at a time. It may work good."
  {:algorithm "Bubble Sort"
   :complexity
   {:best "O(n)"
    :average "O(n^2)"
    :worst "O(n^2)"}}
  (letfn [(should-swap? [a coll]
            (or (empty? coll)
                (->> coll peek (compare a) (> 0))))
          (bubble-swap [acc x]
            (if (seq acc)
              (if-let [top (peek acc)]
                (conj (pop acc) x top)
                (conj [] x))
              [x]))
          (bubble [xs]
            (loop [xs xs acc []]
              (if (seq xs)
                (let [[fs] xs]
                  (if (should-swap? fs acc)
                    (recur (rest xs) (bubble-swap acc fs))
                    (recur (rest xs) (conj acc fs))))
                acc)))]
    (loop [xs xs]
      (let [bubbled (bubble xs)]
        (if (= xs bubbled)
          bubbled
          (recur bubbled))))))


(def algorithms [bubble-sort])

         

