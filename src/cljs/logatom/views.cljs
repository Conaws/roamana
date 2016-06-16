(ns logatom.views
  (:require [reagent.core    :as r]
            [logatom.logger  :as log :refer [conn logatom]]
            [cljs.spec        :as s]
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



(defn submit-form [val]
  (when-let [c @val]
   (log/t! conn [c])
   (js/alert c)
   (reset! val "")))



(defn todo-create []
  (let [r (r/atom {:todo/text "" :todo/done false})]
    (fn []
      [:div
       [input-field "todo" :todo/text r]
       [:button {:on-click #(submit-form r)}
        "Add"]])))



(defn main-view []
  (fn []
    [:div
     [:h1 "CONN"]
     [:div (pr-str @@conn)]
     [:h1 "Logatom"]
     [:div (pr-str @logatom)]]))


