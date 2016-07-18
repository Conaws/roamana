(ns roamana.basic.multiembed
  (:require [reagent.core  :as r  :refer [atom]])
  (:require-macros
            [devcards.core   :as dc
             :refer [defcard defcard-doc defcard-rg deftest]]))

(def app-state
  (r/atom {:columns [{:id 0
                      :title "Todos"
                      :cards [{:id 10
                               :title "Learn about Reagent"}
                              {:id 20
                               :title "Tell my friends about Lambda Island"}]}
                     {:id 1
                      :title "Awesomize"
                      :cards [{:id 11
                               :title "Meditate"}
                              {:id 21
                               :title "Work out"}]}]}))


(defn- update-title [cursor title]
  (swap! cursor assoc :title title))

(defn- stop-editing [cursor]
  (swap! cursor dissoc :editing))

(defn- start-editing [cursor]
  (swap! cursor assoc :editing true))



(defn AutoFocusInput [props]
  (r/create-class {:displayName "AutoFocusInput"
                   :component-did-mount (fn [component]
                                          (.focus (r/dom-node component)))
                   :reagent-render (fn [props]
                                     [:input props])}))



(defn Editable [el cursor]
  (let [{:keys [editing title]} @cursor]
    (if editing
      [el {:className "editing"} 
       [AutoFocusInput {:type "text"
                        :value title
                        :on-change #(update-title cursor (.. % -target -value))
                        :on-blur #(stop-editing cursor)
                        :on-key-press #(if (= (.-charCode %) 13)
                                         (stop-editing cursor))}]]
      [el {:on-click #(start-editing cursor)} title])))



(defcard-rg  carda
  [:div
   [Editable :div.column (r/cursor app-state [:columns 0] ) ]
   [Editable :div.card (r/cursor (r/cursor  app-state [:columns 0]) 
                                   [:cards 0]) ]
   [Editable :div.card (r/cursor app-state [:columns 1] ) ]]
  app-state
  {:inspect-data true
   :history true})





