
(ns logatom.handlers
  (:require [reagent.core    :as r]
            [re-frame.core   :refer [register-handler dispatch]]
            [logatom.logger :refer [schema]]
            [datascript.core :as d]
            [posh.core       :as posh  :refer [pull posh! q transact!]]
            [cljs.pprint     :refer [pprint]]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      collect-one comp-paths] :as sp]
            [clojure.string  :as str])
  (:require-macros
           [com.rpl.specter.macros  :refer [select transform declarepath providepath]]
           [reagent.ratom :refer [reaction]]))




(defn transact-log [db logpath conn & ents]
  (let [txs (d/transact! @conn ents)
        datoms (:tx-data txs)
        txid   (nth (first datoms) 3)]
    (do
      (js/console.log txid)
      (assoc-in db [logpath (js/Date.)] {:datoms datoms :visible true}))))



(register-handler
 :tlog!
 (fn [db [_ conn ents]]
   (transact-log db :log conn ents)))




(register-handler
 :toggle-log-visible
 (fn [db [_ tx conn]]
   (do
     (dispatch [:reset-conn conn])
     (update-in db [:log tx :visible] not))))




(defn conn-from-log [logatom]
  (d/conn-from-datoms (select [MAP-VALS  #(= true (:visible %)) :datoms ALL] logatom) schema))



(register-handler
 :reset-conn
 (fn [db [_ conn]]
 (do  
   (reset! conn (doto (conn-from-log (:log db)) posh!))
   (js/console.log (pr-str @@conn)) 
   db)))
