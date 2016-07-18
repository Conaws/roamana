(ns roamana.basic.ents
  (:require [reagent.core  :as r  :refer [atom]]
            [cljs.pprint :refer [pprint]]
            [re-frame.core :refer [subscribe register-sub]]
            [posh.core :as posh])
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [devcards.core   :as dc
    :refer [defcard-rg]]))


(register-sub
 ::all
 (fn [db]
   (reaction @db)))


(register-sub
 ::all-ents
 (fn [db]
   (let [conn (:ds @db)]
     (posh/q conn '[:find (pull ?e [*])
                     :where  [?e]]))))

(register-sub
 ::sorted-ents
 (fn [db [_ sort-fn]]
   (let [conn (subscribe [::all-ents])
         sortt (or sort-fn (fn [[v]] (:db/id v)))]
     (reaction (sort-by sortt @conn)))))



(defn db-vals []
  (let [all (subscribe [::all])]
  (fn []
    [:div 
     (for [[v k] (dissoc  @all :ds)]
       ^{:key v} [:div  [:h4 (pr-str v)]
            [:li (pr-str k)]])])))





(defn sorted-ents []
  (let [es (subscribe [::sorted-ents nil])]
    (fn []
      [:div {:style
             {:display "grid"
              :grid-row-gap "5px"
              :grid-template-rows "repeat(auto, [row] 1fr"}}
       (for [e @es]
         ^{:key e} 
         [:div
          (pr-str  e)])])))

(defcard-rg sortent
  [sorted-ents])


(defcard-rg ents
  [db-vals])

