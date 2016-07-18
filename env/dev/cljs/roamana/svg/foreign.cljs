(ns roamana.svg.foreign
  (:require [reagent.core  :refer [atom]])
  (:require-macros
            [devcards.core   :as dc
             :refer [defcard defcard-doc defcard-rg deftest]]))


(defonce position1 
  (atom {:state {:x 1 :y 1} :rwidth 200 :rheight 100 :moving false :start {:x 1 :y 1}}))


(defonce position2 
  (atom @position1))



(defn log-position [evt]
  (prn "motion: "(.-clientX evt)(.-clientY evt)))


(defn merge- [a b]
  (merge-with - a b))


(defn start-position [evt ratom]
  (do 
    (prn "motion: "(.-clientX evt)(.-clientY evt)
    (swap! ratom assoc :start (merge- {:x (.-clientX evt) :y (.-clientY evt)} (:state @ratom)))  
    (prn @ratom))))


(defn change-position [evt ratom]
  (let [x (.-clientX evt)
        y (.-clientY evt)
        diff (merge-with - {:x x :y y} (:start @ratom))]
  (do 
    (prn "motion: difference" diff)
    (swap! ratom assoc :state diff)  
    (prn @ratom))))


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
             fo]]))))




(defn layout []
	[:svg 
		{:height 3000
		 :width 3000}
         [fo position1 [:div
                        {:style {:background-color "blue"
                                 :width "600px"}}
                        [:h1 "text"]
                        [:textarea
                         {:style {:width "500px"}}]]]
         [fo position2 [:h1
                        {:style {:background-color "green"
                                 :width "200px"} }
                        "hey"]]])



(defcard-rg layout-card
  "hey"
  [layout])
