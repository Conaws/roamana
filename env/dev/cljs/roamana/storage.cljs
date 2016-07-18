(ns roamana.storage
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [datascript.transit :as dt]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      if-path END cond-path
                                      must pred keypath
                                      collect-one comp-paths] :as sp]
            [roamana.logger :refer [all-ents]]
            [roamana.views :refer [main-view logmap todo-create] :as views]
            [reagent.session :as session]
            [keybind.core :as key]
            [cljs.spec  :as s]
            [cljs.pprint :refer [pprint]]
            [roamana.zz :refer [cursify]]
            [roamana.zz3 :as zz3]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select setval defnav 
                                    defpathedfn
                                    defnavconstructor
                                    fixed-pathed-nav
                                    variable-pathed-nav
                                    transform declarepath providepath]]
   [reagent.ratom :refer [reaction]]
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

;(re-frame.utils/set-loggers! {:warn #(js/console.log "")})


(cljs.reader/register-tag-parser!  "datascript/DB"  datascript.db/db-from-reader)

(cljs.reader/register-tag-parser!  "datascript/Datom"  datascript.db/datom-from-reader)

(enable-console-print!)

(def schema {:node/children {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}})


(defonce conn (d/create-conn schema))
(posh! conn)
(def cc (cursify conn))




(defn store! [k v]
  (js/localStorage.setItem k v))

(defn load! [k]
  (js/localStorage.getItem k))


(defn save-with-transit [kstr db]
  (let [ds (:ds db)]
    (->> (assoc db :ds (dt/write-transit-str @ds))
         pr-str
         (js/localStorage.setItem kstr))))






(defn save-app-to [k db]
  (->> (transform [:ds] #(pr-str @%) db)
       pr-str
       (store! k))
  db)

(d/listen! conn :persistence
           (fn [tx-report]
             (when-let [db (:db-after tx-report)]
               (js/setTimeout #(save-app-to "app-db" @app-db) 0))))






(def default-transaction [{:db/id 0 :node/type :root :node/children #{1 2}}
                          {:db/id 1 :node/type :text :node/text "Main 1"  
                           :node/children #{3}} 
                          {:db/id 2 :node/type :text :node/text "Main 2"
                           :node/children #{3}} 
                          {:db/id 3 :node/type :text :node/text "Main 1 &2 : Child 1"
                           :node/children #{4}} 
                          {:db/id 4 :node/type :text :node/text "Child 1: Grandkid 1"
                           :node/children #{5}}
                          {:db/id 5 :node/type :text :node/text "Grandkid 1 : Great 1"}] )





(register-handler
 ::init-ds
 (fn [db [_ conn]]
   (assoc db :ds  conn)))


(dispatch [::init-ds conn])

#_(dispatch [::init-ds conn])

(def ds-db? #(instance? datascript.db/DB %))
(def ratom? (partial instance? reagent.ratom/RAtom))
(def atom? (partial instance? cljs.core/Atom))

(s/def ::ds  ds-db?)






;; replace this with something that saves the whole state







#_(->> @app-db
     (transform [:ds] #(pr-str @%))
     pr-str
     cljs.reader/read-string
     :ds
     cljs.reader/read-string)


(register-handler
 ::clear-db
 (fn [_]
   (d/transact! conn default-transaction)
   {:ds conn}))




(if-let [db (load! "app-db")]  
  (->> db
   cljs.reader/read-string
   :ds
   cljs.reader/read-string
   (d/reset-conn! conn))
  (dispatch [::clear-db]))
  

(defcard-rg zzgrid
  [zz3/gridview2])



#_(s/fdef load-app
        :args (s/cat :old-app map? 
                     :key string?))


#_(defn merge-app [old-app app-string]
  (let [old-conn (:ds old-app)
        new-ds (:ds (cljs.reader/read-string))]
    #_(assoc db :ds 
           (d/reset-conn! old-conn
                          (dt/read-transit-str ds)))))





;(js/localStorage.getItem "a")




