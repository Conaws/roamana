(ns roamana.views
  (:require [reagent.core    :as r]
            [cljs.spec        :as s]
            [re-frame.core   :refer [register-sub 
                                     subscribe dispatch register-handler]]
            [datascript.core :as d]
            [posh.core       :as posh  :refer [pull posh! q transact!]]
            [cljs.pprint     :refer [pprint]]
            [alandipert.storage-atom :refer [local-storage]]
            [cljs.reader]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      collect-one comp-paths] :as sp]
            [clojure.string  :as str])
  (:require-macros
           [com.rpl.specter.macros  :refer [select transform declarepath providepath]]
           [reagent.ratom :refer [reaction]]))



(defn input-field [placeholder cursor val]
  [:input {:type "text"
           :placeholder placeholder
           :value (cursor @val)
                :on-change #(swap! val assoc cursor (-> % .-target .-value))
                :on-key-down #(when (= (.-key %) "Enter")
                               (js/alert @val))}])




(defn todo-create [conn]
  (let [r (r/atom {:todo/text ""})]
    (fn [conn]
      [:div
       [input-field "todo" :todo/text r]
       [:button {:on-click #(dispatch [:tlog! conn @r])} 
        "Add"]])))




(defn logmap [conn]
  (let [logmap (subscribe [:log])]
        (fn [conn]
          [:div
           (for [[i [tx val]] (map-indexed vector @logmap)]
             [:div
              [:button
               {:on-click #(dispatch [:toggle-log-visible tx conn])} 
               (str i " " (:visible val))]
              [:span (for [[e a v t] (:datoms val)]
                           [:span (str "Added "v " as "a" for " e)])]])])))


;; possible that any sub with conn as value is getting reloaded more than needs to
;; maybe not, if only refreshes if reactoin changes

(defn e [eid conn]
  (let [ent (subscribe [:e eid conn])]
    (fn [ent]
      [:div (pr-str @@ent)])))


(defn main-view [conn]
  (let [c (subscribe [:db-entities conn])
        log (subscribe [:log])]
  (fn [c log conn]
    [:div
     [logmap conn]
     [:div (for [[todo] @@c]
             [e todo conn])]
     [:button {:on-click #(dispatch [:reset-conn conn])} "Reset"]])))
