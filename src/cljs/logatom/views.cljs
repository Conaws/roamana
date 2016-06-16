(ns logatom.views
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
  (let [r (r/atom {:todo/text "" :todo/done false})]
    (fn []
      [:div
       [input-field "todo" :todo/text r]
       [:button {:on-click #(dispatch [:tlog! conn @r])} 
        "Add"]])))


;; possible that any sub with conn as value is getting reloaded more than needs to
;; maybe not, if only refreshes if reactoin changes

(defn main-view [conn]
  (let [c (subscribe [:db-entities conn])
        log (subscribe [:log])]
  (fn []
    [:div
     [:h1 "CONN"]
     [:div (pr-str @c)]
     [:h1 "Logatom"]
     [:div (pr-str @log)]])))
