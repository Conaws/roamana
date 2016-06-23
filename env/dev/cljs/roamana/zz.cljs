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
  #_{:inspect-data true
   :history true})




#_(def schema {
             :node/text    {:db/unique :db.unique/identity}
             :node/out-edge         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
             :edge/to               {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}})


(def schema {:node/children {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}})

#_(def schema2 {:person/name {:db/index true}})

(defonce conn2 (d/create-conn schema))

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
  (key/bind! "n" ::new #(d/transact! conn2 [{:db/id -1 :node/text "boo"}])))


#_(d/transact! conn2 [{:db/id -1 :node/text "hey"}])


(defcard-rg test-cursor
 [:div
  [:button {:on-click #(ds-fns conn2)} "JO"]
  "hey"]
  cc2
  #_{:inspect-data true
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
         ^{:key e}[node catom i e])])))


#_(defcard-rg test-selections0
 [selects catom conn2]
  cc2
  #_{:inspect-data true
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



(register-sub
 :active-entity 
 (fn [db]
   (reaction (:active-entity @db 0))))


(defn add-child [db [_ conn]]
   (let [current (subscribe [:active-entity])]
     (d/transact! conn [{:db/id -1
                         :node/text "New Node"}
                        [:db/add @current :node/children -1]]) 
     db))




(register-handler
 :add-child
 add-child
 )

     

(register-handler
 :reset-keys
 (fn [db [_ conn]]
   (let [cursor (subscribe [:cursor])]
     #_(dispatch [:move-cursor cursor])
     (dispatch [:assoc :editing false])
     (key/bind! "j" ::up      #(dispatch [:move-cursor (inc @cursor)]))
     (key/bind!  "c" ::cursor #(js/alert @cursor))
     (key/bind!  "e" ::edit #(dispatch [:edit-mode conn]))
     (key/bind! "k" ::down    #(dispatch [:move-cursor (dec @cursor)]))
     (key/bind! "i" ::child  #(dispatch [:add-child conn]))
     (key/bind! "n" ::new     #(d/transact! conn [{:db/id -1 :node/text "untitled"}]))
     db)))





(register-sub
 :cursor
 (fn [db]
   (reaction (:cursor @db 0))))

(register-sub
 :edit-mode
 (fn [db]
   (reaction (:editing @db false))))


(defn node1 [i e conn] 
  (let [catom (subscribe [:cursor])
        editing? (subscribe [:edit-mode])]
    (fn [i e]
      (if (= @catom i)
        (dispatch [:assoc :active-entity e]))
      (if (and  (= @catom i) @editing?)
        [:input
         {:auto-focus "auto-focus"}]
        [:div        
         {:style 
          {:background-color (if (= @catom i)
                               "green"
                               "grey")}}
         (pr-str e)]))))




(defn selects1 [conn]
  (let [es (posh/q conn '[:find ?e
                          :where [?e]])]
    (fn []
      [:div    
       [:button {:on-click #(dispatch [:reset-keys conn])} "SETUP"]
       (for [[i [e]] (map-indexed vector @es)]
        ^{:key e}[node1 i e conn])])))


#_(defcard-rg test-selections1*
 [selects1 conn2]
  cc2
  #_{:inspect-data true
   :history true})






(register-handler
 :edit-text
 (fn [db [_ conn]]
   (let [current (subscribe [:active-entity])
         text (subscribe [:key :text])]
     (d/transact! conn [[:db/add @current :node/text @text]])
     (dispatch [:reset-keys conn])
     db)))






(register-handler
 :edit-mode
 (fn [db [_ conn]]
   (let [e (subscribe [:active-entity])
         text (posh/pull conn '[*] @e)]
      (key/unbind-all!)
      (dispatch [:assoc :editing true])
      (dispatch [:assoc :text (:node/text @text)])
      #_(js/alert (str "Editing " @e ))
      (key/bind! "enter" ::edit #(dispatch [:edit-text conn]))
      (key/bind! "esc" ::normal #(dispatch [:reset-keys conn]))
   db)))





(defn node2 [i e conn] 
  (let [catom (subscribe [:cursor])
        editing? (subscribe [:edit-mode])
        text  (subscribe [:key :text])
        node (posh/pull conn '[*] e)]
    
    (fn [i e]
      (if (= @catom i)
        (dispatch [:assoc :active-entity e]))
      (if (and  (= @catom i) @editing?)
        [:div
         [:input
          {:value @text
           :style {:width "100%"}
           :auto-focus "auto-focus"
           :on-change #(dispatch [:assoc :text (-> % .-target .-value)])}]]
        [:div        
         {:style 
          {:display "flex"
           :align-items "center"
         ;  :flex-direction "row"
           :border "1px solid grey"
           :justify-content "space-between"
           :background-color (if (= @catom i)
                               "green"
                               "white")}
          :on-click #(dispatch [:move-cursor i])}
         [:div
          {:style {:max-width "50%"}}
          (:node/text @node)]
         (if-let [children (:node/children @node)]
           [:div  
            {:style {:border "1px solid red"
                    :background-color "white"
                                        ; :width "10%"
                     ;:display "flex"
                     :margin-left "auto"
;                     :flex-grow 1
                    ; :align-self "flex-end"
                     }}
            (count children)]
           #_(for [c (:node/children @node)]
             ^{:key c}[:div (pr-str c)]))]))))




(defn selects2 [conn]
  (let [es (posh/q conn '[:find ?e
                          :where [?e]])]
    (fn []
      [:div    
       [:button {:on-click #(dispatch [:reset-keys conn])} "SETUP"]
       (for [[i [e]] (map-indexed vector @es)]
        ^{:key e}[node2 i e conn])])))


#_(defcard-rg test-selections2*
 [selects2 conn2]
  cc2
  #_{:inspect-data true
   :history true})






(defn node3 [i e conn] 
  (let [catom (subscribe [:cursor])
        editing? (subscribe [:edit-mode])
        text  (subscribe [:key :text])
        node (posh/pull conn '[*] e)]
    
    (fn [i e]
      #_(if (= @catom i)
        (dispatch [:assoc :active-entity e]))
     ; (if false (and  (= @catom i) @editing?)
        #_[:div
         [:input
          {:value @text
           :style {:width "100%"}
           :auto-focus "auto-focus"
           :on-change #(dispatch [:assoc :text (-> % .-target .-value)])}]]
        [:div        
         {:style 
          {:border "1px solid grey"
           :background-color (if (= @catom i)
                               "green"
                               "blue")}}
         (:node/text @node)
         (if-let [children (:node/children @node)]
           (count children)
           #_(for [c (:node/children @node)]
             ^{:key c}[:div (pr-str c)]))])))
;)


(d/q '[:find ?e
       :in $
       :where [?p :node/text ?text]
              [_ :node/children ?e]
    ;   [(not= ?e ?p)]
       #_[(not= ?text  "untitled")]]
 @conn2)



(register-sub
 :children
 (fn [_ [_ conn] [ae]]
     (posh/q conn '[:find (pull ?e [*])
                    :in $ ?parent
                    :where  [?parent :node/children ?e]]
             ae)))


(defn children1 [conn]
  (let [active (subscribe [:active-entity]) 
        children (subscribe [:children conn][active])]
    (fn []
      [:div
       {:style {:width "50%"
                :display "flex"
                :justify-content "center"
                :flex-direction "column"}}
       #_(pr-str @children)
       (for [[i [e]] (map-indexed vector @children)]
           ^{:key e}[:div (pr-str e)])])))


(defn selects3 [conn]
  (let [e (subscribe [:active-entity])
        es (posh/q conn '[:find ?p
                          :where 
                          [?p :node/text _]])
        kids (posh/q conn '[:find ?p
                            :where [_ :node/children ?p]])]
    (fn []
      (let [roots (clojure.set/difference @es @kids)]
      [:div    
       {:style {:display "flex"
                :flex-direction "row"}}
       [:button {:on-click #(dispatch [:reset-keys conn])} "SETUP"]
       [:div 
        {:style {:width "50%"
                 :display "flex"
                 :flex-direction "column"}}
        (for [[i [e]] (map-indexed vector (sort roots))]
          ^{:key e}[node2 i e conn])]
       [children1 conn]]))))
        


(defcard-rg test2
 [selects3 conn2]
  cc2
  {:inspect-data true
   :history true})



(comment
(defmulti panels identity)
(defmethod panels :home-panel [] [home-panel])
(defmethod panels :about-panel [] [about-panel])
(defmethod panels :default [] [:div])




(defn main-panel []
  (let [active-panel (subscribe [:active-panel])]
    (fn []
      [:div
       (panels @active-panel)]))))





(def cursor-vec (atom {:depth 1
                       :cursor [1 2 3 4]
                       :lists [[1 2 3 4 5]
                               [1 2 3 4 5]
                               [1 2 3 4 5]
                               [1 2 3 4 5]]}))



#_(swap! cursor-vec (fn [m] (transform [:cursor #(nth % 3) LAST] inc m)))



(defn inc-cursor [db]
  (let [d (:depth db)
        l (:lists db)
        count (-> (nth l d)
                  count)]
    (transform [:cursor (sp/srange d (inc d))] 
               (partial map 
                        (fn [value]
                          (if (< count (inc value))
                            1
                            (inc value)))) db)))


(defn dec-cursor [db]
  (let [d (:depth db)
        l (:lists db)
        count (-> (nth l d)
                  count)]
    (transform [:cursor (sp/srange d (inc d))] 
               (partial map 
                        (fn [value]
                          (if (>= 0 (dec value))
                            count
                            (dec value)))) db)))


#_(def inc-cursor (partial adjust-cursor inc))
#_(def dec-cursor (partial adjust-cursor dec))


@cursor-vec

(swap! cursor-vec inc-cursor)

(defn vec-keys [atom]
  (key/bind! "l" ::left #(swap! atom update :depth inc))
  (key/bind! "h" ::right #(swap! atom update :depth dec))
  (key/bind! "j" ::down  #(swap! atom dec-cursor))
  (key/bind! "k" ::up  #(swap! atom inc-cursor))
)


(defn vecview [atom]
  (let [depth (subscribe [:cursor])]
    (fn []
      [:div {:style {:display "flex"
                     :flex-direction "row"}}
       (pr-str @depth)
       [:button {:on-click #(vec-keys atom)}]
       (for [[d column] (map-indexed vector (:lists @atom))]
         [:div
          {:style {:flex-grow 1
                   :border (if (= d (:depth @atom))
                             "2px solid red"
                             "1px solid black")}}
          (for [row column]
            [:div 
             (if (and 
                  (= row (nth (:cursor @atom) d)))
               [:h2 (pr-str (nth (:cursor @atom) d))]
               (pr-str row))])])])))


(defcard-rg cursortes*t
  "hey"
  [vecview cursor-vec]
  cursor-vec
  {:inspect-data true})
