(ns roamana.storage1
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [datascript.transit  :as dt]
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
            [roamana.zz :refer [cursify]]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select setval defnav 
                                    defpathedfn
                                    select-one
                                    defnavconstructor
                                    fixed-pathed-nav
                                    variable-pathed-nav
                                    transform declarepath providepath]]
   [reagent.ratom :refer [reaction]]
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(re-frame.utils/set-loggers! {:warn #(js/console.log "")})

(def schema {:node/children {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}})

(defonce conn (d/create-conn schema))

(posh! conn)


(defn persist [db]
  (js/localStorage.setItem "datascript/DB" (dt/write-transit-str db)))


(d/listen! conn :persistence
           (fn [tx-report]
             (when-let [db (:db-after tx-report)]
               (js/setTimeout #(persist db) 0))))






(def ds-db? #(instance? datascript.db/DB %))

(s/def ::ds  ds-db?)



(register-handler
 ::load-user-graph
 (fn [db [_ conn]]
   (assoc db :user-graph conn)))

(defonce history (atom []))
(def ^:const history-limit 100)




(defn drop-tail [xs pred]
  (loop [acc []
         xs xs]
    (let [x (first xs)]
      (cond
        (nil? x) acc
        (pred x) (conj acc x)
        :else (recur (conj acc x) (next xs))))))





(defn til-end
  ([pred] (partial til-end pred))
  ([pred xs] (count (take-while pred xs))))


(defn my-drop [xs pred]
  (select-one 
   (sp/srange-dynamic 
    (fn [_] 0) 
    (fn [xs] (min (til-end (comp not nil?) xs)
                   (inc  (til-end (comp not pred) xs)))))
   xs))



(d/listen! conn :history
  (fn [tx-report]
    (let [{:keys [db-before db-after]} tx-report]
      (when (and db-before db-after)
        (swap! history (fn [h]
          (-> h
            (drop-tail #(identical? % db-before))
            (conj db-after)
            #_(u/trim-head history-limit))))))))






(register-handler
 ::add-ds
 (fn [db [_ conn]]
   (assoc db :ds conn)))


(dispatch [::add-ds conn])



(register-handler
 ::transact
 (fn [db [_ transaction & {:keys [ds-id] :or {ds-id :ds}}]]
   (let [conn (:ds db)]
    ; (js/alert  (pr-str conn))
    ; (js/alert (d/transact! conn transaction))
     db)))




(dispatch [::transact [{:db/id 0 :node/type :root :node/children #{1 2}}
                       {:db/id 1 :node/type :text :node/text "Main 1"  
                        :node/children #{3}} 
                       {:db/id 2 :node/type :text :node/text "Main 2"
                        :node/children #{3}} 
                       {:db/id 3 :node/type :text :node/text "Main 1 &2 : Child 1"
                        :node/children #{4}} 
                       {:db/id 4 :node/type :text :node/text "Child 1: Grandkid 1"
                        :node/children #{5}}
                       {:db/id 5 :node/type :text :node/text "Grandkid 1 : Great 1"} ]])






(register-sub
 ::all
 (fn [db]
   (reaction @db)))


(register-sub
 ::conn
 (fn [db]
   (reaction (:ds @db))))


(register-sub
 ::all-ents
 (fn [db]
   (let [conn (:ds @db)]
     (posh/q conn '[:find (pull ?e [*])
                     :where  [?e]]))))


(defn ents []
  (let [es (subscribe [::all-ents])
        all (subscribe [::all])]
  (fn []
    [:div 
     [:div (pr-str @es)]
     [:div (pr-str @all)]])))



(defcard-rg ents
  [ents])

(key/bind! "cmd-i" :create #(dispatch [::transact [[:db/add -1 :name "yo"]]]))







(defn save-load [{:keys [save load]}]
  (key/bind! save  ::save-k #(dispatch [::save-me]))
  (key/bind! load ::load-k #(dispatch [::load-me]))
)


(register-handler
 ::edit
 (fn [db [_ v]]
   (assoc db :edit v)))


(save-load {:save "s" :load "cmd-h"})

(register-sub
 ::text
 (fn [db]
   (reaction (:edit @db))))

(defn tester []
  (let [v (subscribe [::text])]
    (fn []
      [:div
       [:h1 @v]
       [:input {:value @v
                :on-change #(dispatch [::edit (-> % .-target .-value)])}
        ]])))


(defcard-rg test1
  [tester])
