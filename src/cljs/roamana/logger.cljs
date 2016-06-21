(ns roamana.logger
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



(def jim2 [[2 :name "Jimmy" 1123 true][2 :name "Jim" 1111 false][2 :name "Jim" 1100 true]])





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



(defn index [xs]
  (map vector xs (range)))

(defn e-by-av [db a v]
  (-> (d/datoms db :avet a v) first :e))




(defn all-ents [db]
  (-> (d/pull-many db '[*]
        (select [ALL ALL]
                (d/q '[:find ?e :in $ :where [?e]] db)))
      pprint))











