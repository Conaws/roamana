(ns roamana.zz
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      collect-one comp-paths] :as sp]
            [roamana.logger :refer [conn]]
            [roamana.views :refer [main-view logmap todo-create] :as views]
            [reagent.session :as session]
            [keybind.core :as key]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select setval transform declarepath providepath]]
   [reagent.ratom :refer [reaction]]
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))



(defn some-mount-function [items current-item]
  (key/bind! "j" ::next #(js/alert items current-item))
  (key/bind! "shift-space" ::prev #(js/alert current-item))
  (key/bind! "C-c C-x j" ::chord #(js/alert "ever heard of emacs chords?")))

(defn some-unmount-function []
  (key/unbind! "j" ::next)
  (key/unbind! "shift-space" ::prev)
  (key/unbind! "C-c C-x j" ::chord)
  ;; or simply:
  (key/unbind-all!))









(defcard-rg hey
[:div
 [:button {:on-click #(some-mount-function [1 2 3 4] 1)} "BIND ING"]
 [:button {:on-click #(key/unbind-all!)} "DISSSS MOUNT"]])





(def blocks (atom {:blocks [:red :blue]}))

(declare block-fns)

(defn blockkey [block]
  (fn [block]
    [:div
     [:button {:on-click #(block-fns block)} "blocks"]
     (for [b (:blocks @block)]
       [:div (pr-str b)])]))


;;  @key/BINDINGS  doesn't show up




(defn block-fns [blocks]
  (key/bind! "r" ::next #(swap! blocks update :blocks (fn [m]  
                                                           (conj  m :red))))
  (key/bind! "b" ::prev #(reset! blocks
                                (setval [:blocks LAST] :bluer @blocks)))
  (key/bind! "ctrl-c ctrl-x j" ::chord #(js/alert "ever heard of emacs chords?")))


(defcard-rg blocks
  [blockkey blocks]
  blocks
  {:inspect-data true
   :history true})






(def schema2 {:person/name {:db/index true}})

(defonce conn2 (d/create-conn))

(posh! conn2)



(defn cursor [x getter & [setter]]
  (when-not (satisfies? cljs.core/IDeref x)
    (throw (js/Error.
            "Argument to |cursor| must be an IDeref")))
  (reify
    cljs.core/IDeref
      (-deref [this] (getter @x))
    cljs.core/IAtom
    cljs.core/IReset
      (-reset! [this new-value]
        (reset! x new-value))
    cljs.core/IWatchable
      (-notify-watches [this oldval newval]
        (doseq [[key f] (.-watches x)]
          (f key this oldval newval))
        this)
      (-add-watch [this k f]
        (add-watch x k f)
        this)
      (-remove-watch [this k]
        (remove-watch x k)
        this)))


(defn db->seq
  {:todo ["Add for db"]}
  [db]
  (->> db
       (map (fn [d] [:e     (:e     d)
                     :a     (:a     d)
                     :v     (:v     d)
                     :tx    (:tx    d)
                     :added (:added d)
                     ]))))


(defn cursify [conn]
  (cursor conn #(-> % db->seq)))


(def cc2 (cursify conn2))


(defn ds-fns [conn2]
  (key/bind! "n" ::new #(d/transact! conn2 [{:db/id -1 :text "boo"}])))


(d/transact! conn2 [{:id 1 :text "hey"}])


(defcard-rg test-cursor
 [:div
  [:button {:on-click #(ds-fns conn2)} "JO"]
  "hey"]
  cc2
  {:inspect-data true
   :history true})




(defonce catom (atom 1))

(defn navkeys [cursor conn2]
  (js/alert @cursor @conn2)
  (key/bind! "j" ::new #(swap! cursor inc))
  (key/bind! "k" ::new #(swap! cursor dec))
  (key/bind! "n" ::new #(d/transact! conn2 [{:db/id -1 :text "boo"}])) )




(defn node [catom i e]
    [:div
     {:style {:background-color (if (= @catom i)
                                  "red"
                                  "grey")}}
     (pr-str e)])


(defn selects [catom conn]
  (let [es (posh/q conn '[:find ?e
                          :where [?e]])]
    (fn []
      [:div    
       [:button {:on-click #(navkeys catom conn)} "JO"]
       (for [[i [e]] (map-indexed vector @es)]
         [node catom i e])])))


(defcard-rg test-selections0
 [selects catom conn2]
  cc2
  {:inspect-data true
   :history true})



(register-handler
 :move-cursor
 (fn [db [_ pos]]
   (assoc db :cursor pos)))



(register-sub
 :key
 (fn [db [_ k]]
   (reaction (get @db k))))


(register-handler
 :assoc
 (fn [db [_ k v]]
   (assoc db k v)))



(register-handler
 :add-child
 (fn [db [_ conn]]
   (let [current-position (subscribe [:key :current-position])]
     (js/alert @current-position)
     db)))
     

(register-handler
 :reset-keys
 (fn [db [_ conn]]
   (let [cursor (subscribe [:cursor])]
     (dispatch [:move-cursor 0])
     (key/bind! "j" ::up      #(dispatch [:move-cursor (inc @cursor)]))
     (key/bind!  "c" ::cursor #(js/alert @cursor))
     (key/bind! "k" ::down    #(dispatch [:move-cursor (dec @cursor)]))
     (key/bind! "i" ::child  #(dispatch [:add-child conn]))
     (key/bind! "n" ::new     #(d/transact! conn [{:db/id -1 :node/text "untitled"}]))
     db)))





(register-sub
 :cursor
 (fn [db]
   (reaction (:cursor @db))))


(defn node1 [i e conn] 
  (let [catom (subscribe [:cursor])]
    (fn []
      (if (= @catom i)
        (dispatch [:assoc :current-position e]))
      [:div
       {:on-click #(do
                     (js/alert "hey")
                     (dispatch [:assoc :current-position e]))
        :style 
        {:background-color (if (= @catom i)
                                    "green"
                                    "grey")}}
       (pr-str e)])))




(defn selects1 [conn]
  (let [es (posh/q conn '[:find ?e
                          :where [?e]])]
    (fn []
      [:div    
       [:button {:on-click #(dispatch [:reset-keys conn])} "SETUP"]
       (for [[i [e]] (map-indexed vector @es)]
         [node1 i e conn])])))


(defcard-rg test-selections1
 [selects1 conn2]
  cc2
  {:inspect-data true
   :history true})






