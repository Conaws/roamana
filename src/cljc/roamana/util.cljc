(ns roamana.util
  (:require
    [cljs.spec :as s]
    [clojure.string :as str])
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


(defn count-tabs
  [string]
  (count (take-while #{\tab} string)))


(def sstring "nodeA\n\tnode1\n\t\tnodeB\n\n\n\t\tnodeC")

(defn parsed [text]
    (->> (str/split text #"\n")
         (map (juxt count-tabs str/trim))
         (filter #(not (empty? (second %))))
))



(defmacro s! [a & fns]
  `(swap! ~a 
          (fn [m#]
            (->> m# ~@fns))))



(defmacro count-till [s f]
  `(count (take-while #(not (f %)))))


(defmacro rev [h & g]
  (cons h (reverse g)))




(defn foo-cljc [x]
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
