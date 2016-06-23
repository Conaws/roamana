(ns roamana.complete
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      collect-one comp-paths] :as sp]
            [roamana.logger :refer [conn]]
            [roamana.views :refer [main-view logmap todo-create] :as views]
            [reagent.session :as session]
            [re-complete.core :as re-complete]
            [re-complete.dictionary :as dictionary]
            [keybind.core :as key]
            [clojure.string :as string]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select setval transform declarepath providepath]]
   [reagent.ratom :refer [reaction]]
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(register-sub
 :get-list
 (fn
   [db [_ list-name]]
   (reaction (get-in @db [(keyword list-name) :added-items]))))



(register-handler
 :add-item-to-list
 (fn
   [db [_ list-name input]]
   (update-in db [(keyword list-name) :added-items] #(vec (conj % input)))))



(register-handler
 :clear-input
 (fn
   [db [_ linked-component-key]]
   (assoc-in db [:re-complete :linked-components (keyword linked-component-key) :text] "")))




(defn list-view [items]
  (map (fn [item]
         ^{:key item}
         [:li.item item])
       items))



(defn render-list
  ([list-name dictionary]
   (render-list list-name dictionary nil))
  ([list-name dictionary options]
   (let [get-input (subscribe [:get-previous-input list-name])
         get-list (subscribe [:get-list list-name])]
     (dispatch [:options list-name options])
     (dispatch [:dictionary list-name dictionary])
     (fn []
       [:div {:className (str list-name " my-list")}
        [:div {:className "panel panel-default re-complete"}
         [:div {:className "panel-heading"}
          #_[:h1 (string/capitalize (str list-name "s"))]]
         [:div.panel-body
          [:ul.checklist
           [:li.input
            [:input {:type "text"
                     :className "form-control input-field"
                     :placeholder (str list-name " name")
                     :value @get-input
                     :on-change (fn [event]
                                  (dispatch [:input list-name (.. event -target -value)]))
                     :on-focus #(dispatch [:focus list-name true])
                     :on-blur #(dispatch [:focus list-name false])}]
            [:button {:type "button"
                      :className "btn btn-default button-ok"
                      :on-click #(do (dispatch [:add-item-to-list list-name @get-input])
                                     (dispatch [:clear-input list-name]))}
             [:span {:className "glyphicon glyphicon-ok check"}]]]
           (list-view @get-list)]]
         [:div.re-completion-list-part
          [re-complete/completions list-name]]]]))))


(def my-lists [["vegetable" (sort dictionary/vegetables) {:trim-chars "[]()"
                                                          :keys-handling {:visible-items 4
                                                                          :item-height 20}}]
               ["fruit" (sort-by count dictionary/fruits) {:trim-chars "?"
                                                           :case-sensitive? true}]
               ["grain" dictionary/grains]])







(defn recomplete-demo []
  (into [:div.my-app]
        (map #(into [render-list] %) my-lists)))




(defcard-rg demo1
  [recomplete-demo])




(register-sub
 :text
 (fn [db]
   (reaction (:text @db))))

(register-handler
 :change-text
 (fn [db [_ text]]
   (assoc db :text text)))


(register-sub
 :get-list
 (fn
   [db [_ list-name]]
   (reaction (get-in @db [(keyword list-name) :added-items]))))





(defn rec [list-name]
  (let [text (subscribe [:text])
        get-input (subscribe [:get-previous-input list-name])
        get-list (subscribe [:get-list list-name])]
    (fn []
      [:div
       [:div {:className (str list-name " my-list")}
        [:div      {:className "panel panel-default re-complete"}
         [:div     ;{:className "panel-heading"}
          #_[:h1 (string/capitalize (str list-name "s"))]]
         [:div.panel-body
          [:ul.checklist
           [:li.input
            [:input {:type "text"
                     :className "form-control input-field"
                     :placeholder (str list-name " name")
                     :value @get-input
                     :on-change (fn [event]
                                  (dispatch [:input list-name (.. event -target -value)]))
                     :on-focus #(dispatch [:focus list-name true])
                     :on-blur #(dispatch [:focus list-name false])}]
            [:button {:type "button"
                      :className "btn btn-default button-ok"
                      :on-click #(do (dispatch [:add-item-to-list list-name @get-input])
                                     (dispatch [:clear-input list-name]))}
             [:span {:className "glyphicon glyphicon-ok check"}]]]
           (list-view @get-list)]]
         [:div.re-completion-list-part
          [re-complete/completions list-name]]]]])))



(dispatch [:dictionary "Juntoist" '("Mike" "Sanjena" "Conor" "Aniq")])

(defcard-rg juntoists
  [rec "Juntoist"])
