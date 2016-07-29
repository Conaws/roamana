(ns roamana.utils)

(defmacro s! [a fns]
  `(swap! a
          (fn [m#]
            (->> m#
                 fns))))


(defmacro rev [h & g]
  (cons h (reverse g)))


