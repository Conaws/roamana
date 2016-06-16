(ns logatom.logger
  (:require [reagent.core    :as r]
            [cljs.spec        :as s]
            [datascript.core :as d]
            [re-frame.db     :refer [app-db]]
            [re-frame.core   :refer [register-sub 
                                     subscribe dispatch register-handler]]
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



(enable-console-print!)

(def schema {:todo/tags    {:db/cardinality :db.cardinality/many}
             :todo/project {:db/valuetype :db.type/ref}
             :todo/done    {:db/index true}
             :todo/due     {:db/index true}})


(def conn (r/atom (doto
                      (d/create-conn schema)
                    posh!)))


#_(def logatom (local-storage (atom {}) :logatom))


(defn transact-log [db logpath conn & ents]
  (let [txs (d/transact! @conn ents)
        datoms (:tx-data txs)
        txid   (nth (first datoms) 3)]
    (do
      (js/console.log txid)
      (assoc-in db [logpath txid] {:datoms datoms :visible true}))))


(register-handler
 :tlog!
 (fn [db [_ conn ents]]
   (transact-log db :log conn ents)))



(declare clean-datoms)

;; need to either put in clean-datoms in here, or fixup transactor fn
;; either should be spec'd so that transaction ids are in right order

(defn conn-from-log [logatom]
  (d/conn-from-datoms (select [MAP-VALS  #(= true (:visible %)) :datoms ALL] logatom) schema))


(comment
  (select [MAP-VALS #(= true (:visible %)) :datoms ALL] (:log @app-db))
  (conn-from-log (:log @app-db)))




(register-handler
 :reset-conn
 (fn [db [_ conn]]
 (do  
   (reset! conn (doto (conn-from-log (:log db)) posh!))
   (js/console.log (pr-str @@conn)) 
   db)))





(register-sub
 :log
 (fn [db]
   (reaction (:log @db))))

@conn


;;; posh subscriptions
(defn mypull [conn eid]
  (pull @conn '[*] eid))


(register-sub
 :e
 (fn [_ [_ eid] conn]
   (pull @conn '[*] eid)))





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


#_(def jim1  [[:db/retract 2 :name "James"]
           [:db/add 2 :name "James"]
           [:db/add 2 :name "Jim"]])



(def jim2 [[2 :name "Jimmy" 1123 true][2 :name "Jim" 1111 false][2 :name "Jim" 1100 true]])



#_(->> jim2
     (map (partial take 3))
     (map #{'(2 :name "Jim")}))




(defn clean-datoms 
  "requires that datoms are in order of newest first"
  [datoms]
  (-> (reduce (fn [rm test]
                (if (nth test 4)
                  (if (not ((:set rm) (vec (take 3 test))))
                    (update rm :results #(conj % (vec (take 4 test))))
                    rm)
                  (update rm :set #(conj % (vec (take 3 test))))))
              {:set #{} :results []}
              datoms)
      :results))



(defn datoms-via-transaction [datoms]
  (d/with (d/empty-db) 
          (mapv #(concat [(if (nth % 4) :db/add :db/retract)] %) datoms)))



(def transactiondb (d/create-conn (datoms-via-transaction jim2)))



(d/q '[:find ?e ?a ?v
      :where [?e ?a ?v]]
      @transactiondb '[*] 1)






(defn index [xs]
  (map vector xs (range)))

(defn e-by-av [db a v]
  (-> (d/datoms db :avet a v) first :e))






(defn all-ents [db]
  (-> (d/pull-many db '[*]
        (select [ALL ALL]
                (d/q '[:find ?e :in $ :where [?e]] db)))
      pprint))



(def fixtures [
  [:db/add 0 :system/group :all]
  {:db/id -1
   :project/name "datascript"}
  {:db/id -2
   :project/name "nyc-webinar"}
  {:db/id -3
   :project/name "shopping"}
               
  {:todo/text "displaying list of todos"
   :todo/tags ["listen" "query"]
   :todo/project -2
   :todo/done true
   :todo/due  #inst "2014-12-13"}
  {:todo/text "persisting to localstorage"
   :todo/tags ["listen" "serialization" "transact"]
   :todo/project -2
   :todo/done true
   :todo/due  #inst "2014-12-13"}
  {:todo/text "make task completable"
   :todo/tags ["transact" "funs"]
   :todo/project -2
   :todo/done false
   :todo/due  #inst "2014-12-13"}
  {:todo/text "fix fn calls on emtpy rels"
   :todo/tags ["bug" "funs" "query"]
   :todo/project -1
   :todo/done false
   :todo/due  #inst "2015-01-01"}
  {:todo/text "add db filtering"
   :todo/project -1
   :todo/done false
   :todo/due  #inst "2015-05-30"}
  {:todo/text "soap"
   :todo/project -3
   :todo/done false
   :todo/due  #inst "2015-05-01"}
  {:todo/text "cake"
   :todo/done false
   :todo/project -3}
  {:todo/text "just a task" :todo/done false}
  {:todo/text "another incomplete task" :todo/done false}])













#_(add-watch logatom
           :logatom
           (fn [_ _ _ v]
             (.log js/console "Logging" v)))










