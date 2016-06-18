(ns logatom.subs
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





(register-sub
 :log
 (fn [db]
   (reaction (:log @db))))






;;; posh subscriptions
(defn mypull [conn eid]
  (pull @conn '[*] eid))


(register-sub
 :e
 (fn [_ [_ eid conn]]
  (reaction (pull @conn '[*] eid))))






(defn datom-query [conn]
  (q conn '[:find ?e ?attr ?val ?tx
            :where
            [?e ?attr ?val ?tx]]))




(register-sub
 :datoms
 (fn [_ [_ conn]]
   (datom-query @conn)))


(register-sub
 :db-entities
 (fn [_ [_ conn]]
  (reaction (q @conn '[:find ?e
                       :where
                       [?e]]))))


