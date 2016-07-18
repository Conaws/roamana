(ns roamana.foreign
  (:require [reagent.core    :as rx     ]
            [cljs.reader                ]
            [clojure.string  :as str    ])
  (:require-macros
            [reagent.ratom :refer [reaction]]
            [devcards.core   :as dc
             :refer [defcard defcard-doc defcard-rg deftest]]))

(defonce position1 
  (rx/atom {:state {:x 1 :y 1} :rwidth 200 :rheight 100 :moving false :start {:x 1 :y 1}}))


(defonce position2 
  (rx/atom @position1))



(defn log-position [evt]
  (prn "motion: "(.-clientX evt)(.-clientY evt)))


(defn merge- [a b]
  (merge-with - a b))


(defn start-position [evt atom]
  (do 
    (prn "motion: "(.-clientX evt)(.-clientY evt)
    (swap! atom assoc :start (merge- {:x (.-clientX evt) :y (.-clientY evt)} (:state @atom)))  
    (prn @atom))))


(defn change-position [evt atom]
  (let [x (.-clientX evt)
        y (.-clientY evt)
        diff (merge-with - {:x x :y y} (:start @atom))]
  (do 
    (prn "motion: difference" diff)
    (swap! atom assoc :state diff)  
    (prn @atom))))


(defn fo [posatom fo]
  (let [position posatom]
    (fn []
      (let [
            dx (get-in @position [:state :x])
            dy (get-in @position [:state :y])]
         [:g 
          { 
            :on-mouse-down #(do (swap! position assoc :moving true)
                                (start-position % position))
            :on-mouse-up #(do (swap! position assoc :moving false)
                                (log-position %))
            :on-mouse-leave #(do (swap! position assoc :moving false))
            :on-mouse-move #(do (if (:moving @position)
                                      (change-position % position))) } 
            [:foreignObject 
              {:width 1000
              	:height 1000
               :x dx
               :y dy
              }
              (if (:moving @position)
                [:button "Moving"]
              	#_[s/icon {:soda {:icon :spinner
              					:state :loading}}])
              fo
              ]]))))




(defn layout []
	[:svg 
		{:height 3000
		 :width 3000
		 }
		 [fo position1 [:textarea]]
		 [fo position2 [:h1 "hey"]]])

