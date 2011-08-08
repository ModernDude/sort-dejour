(ns sort-dejour.util
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
