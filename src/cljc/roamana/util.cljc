(ns roamana.util
  (:require
   [cljs.spec :as s])
  )


(defn c 
  ([clickfn] (c {} clickfn))
  ([handlermap clickfn]
   (assoc handlermap :on-click clickfn)))

(defn s 
  ([m] (s {} m))
  ([handlermap m]
   (assoc handlermap :style m)))

(def key-name
  {13 "ENTER"
   27 "ESC"
   9  "TAB"
   46 "DELETE"
   8 "BACKSPACE"})


(defmacro keycase [e]
  `(case (.-which ~e)
     9 (js/alert "hey")

     #_(do
         (dispatch [:clear-text
                    (-> % .-target .-selectionStart)
                    (-> % .-target .-selectionEnd)])
         (.preventDefault %))
     :else))







(defmacro s! [a & fns]
  `(swap! ~a 
          (fn [m#]
            (->> m# ~@fns))))



(defmacro count-till [s f]
  `(count (take-while #(not f %))))


(defmacro rev [h & g]
  (cons h (reverse g)))




(defn foo-cljc [x]
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
