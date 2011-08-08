(ns sort-dejour.core
  "Sorting Algorithms"
  (require [clojure.contrib.generic.comparison :as gcmp])
  (require [clojure.contrib.math :as math])
  (require [clojure.contrib.seq-utils :as sequ]))


(defn remove-first
  "Returns a lazy sequence of the items in coll for which
  the first instance in with (pred item) is true is removed/"
  ([pred coll]
     (letfn [(remfirst [pred coll removed?]
               (lazy-seq
                (when-let [s (seq coll)]
                  (let [f (first s) r (rest s)]
                    (if (and (pred f) (false? removed?))
                      (remfirst pred r true)
                      (cons f (remfirst pred r removed?)))))))]
       (remfirst pred coll false))))


(defn partition-by-nth [n coll]
  "Returns a lazy sequence of n lists in which each list is composed
of every nth element in coll. Each list returned will skip the first x items in coll
where x is the current list index starting at 0"  
  (when (seq coll)
    (map #(take-nth n (drop %1 coll)) (range 0 n))))
  


(defn reduce-until [f val coll pred rf]
  "f should be a function of 2 arguments. Returns the result of applying f to the
first two items in coll. f is next appied to the result as well as teh next value in coll. This process repeast until coll is empty. The function pred will be applied to te The original coll as well as the result of the reduce. If the pred returns true, the result of the reduction will be returned. Otherwise, the reduction will be performed on the result. The result will then be passed into the predicated along with the previos result. This process continus until the predicate returns true. "  
  (loop [coll coll]
     (let [xs (reduce f val coll)]
       (if (pred coll xs)
         (rf coll xs)
         (recur xs)))))




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

         

