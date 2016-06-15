(ns logatom.logger
  (:require [reagent.core    :as r]
            [cljs.spec        :as s]
            [datascript.core :as d]
            [posh.core       :as posh  :refer [pull posh! q transact!]]
            [cljs.pprint     :refer [pprint]]
            [alandipert.storage-atom :refer [local-storage]]
            [cljs.reader]
            [com.rpl.specter  :refer [ALL STAY LAST 
                                      stay-then-continue 
                                      collect-one comp-paths] :as sp]
            [clojure.string  :as str])
  (:require-macros
           [com.rpl.specter.macros  :refer [select transform declarepath providepath]]
           [reagent.ratom :refer [reaction]]))



(enable-console-print!)

(def schema {:todo/tags    {:db/cardinality :db.cardinality/many}
             :todo/project {:db/valueType :db.type/ref}
             :todo/done    {:db/index true}
             :todo/due     {:db/index true}})


(defonce conn (doto
                  (d/create-conn schema)
                posh!))



(defn index [xs]
  (map vector xs (range)))

(defn e-by-av [db a v]
  (-> (d/datoms db :avet a v) first :e))


(defn all-ents [conn]
  (-> (d/pull-many @conn '[*]
        (select [ALL ALL]
                (d/q '[:find ?e :in $ :where [?e]] @conn)))
      pprint))



(def fixtures [
  [:db/add 0 :system/group :all]
  {:db/id -1
   :project/name "datascript"}
  {:db/id -2
   :project/name "nyc-webinar"}
  {:db/id -3
   :project/name "shopping"}
               
  {:todo/text "Displaying list of todos"
   :todo/tags ["listen" "query"]
   :todo/project -2
   :todo/done true
   :todo/due  #inst "2014-12-13"}
  {:todo/text "Persisting to localStorage"
   :todo/tags ["listen" "serialization" "transact"]
   :todo/project -2
   :todo/done true
   :todo/due  #inst "2014-12-13"}
  {:todo/text "Make task completable"
   :todo/tags ["transact" "funs"]
   :todo/project -2
   :todo/done false
   :todo/due  #inst "2014-12-13"}
  {:todo/text "Fix fn calls on emtpy rels"
   :todo/tags ["bug" "funs" "query"]
   :todo/project -1
   :todo/done false
   :todo/due  #inst "2015-01-01"}
  {:todo/text "Add db filtering"
   :todo/project -1
   :todo/done false
   :todo/due  #inst "2015-05-30"}
  {:todo/text "Soap"
   :todo/project -3
   :todo/done false
   :todo/due  #inst "2015-05-01"}
  {:todo/text "Cake"
   :todo/done false
   :todo/project -3}
  {:todo/text "Just a task" :todo/done false}
  {:todo/text "Another incomplete task" :todo/done false}])



(d/transact! conn fixtures)


(all-ents conn)


