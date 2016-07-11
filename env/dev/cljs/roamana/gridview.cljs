(ns roamana.gridview
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
            [roamana.zz :refer [cursify]]
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

(def ds-db? #(instance? datascript.db/DB %))
(def ratom? (partial instance? reagent.ratom/RAtom))
(def atom? (partial instance? cljs.core/Atom))

(s/def ::ds  ds-db?)

(cljs.reader/register-tag-parser!  "datascript/DB"  datascript.db/db-from-reader)

(cljs.reader/register-tag-parser!  "datascript/Datom"  datascript.db/datom-from-reader)


(if-let [db (load! "app-db")]
  (->>  db
   cljs.reader/read-string
   :ds
   cljs.reader/read-string
   (d/reset-conn! conn))
  (d/transact! conn default-transaction))
  



(register-sub
 ::conn
 (fn [db]
   (reaction (:ds @db))))



(register-handler
 ::transact
 (fn [db [_ transaction & {:keys [ds-id] :or {ds-id :ds}}]]
   (let [conn (get db ds-id)]
     (d/transact! conn transaction)
     db)))




(register-sub
 ::all
 (fn [db]
   (reaction @db)))


(register-sub
 ::all-ents
 (fn [db]
   (let [conn (subscribe [::conn])]
     (posh/q @conn '[:find (pull ?e [*])
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



;;root is hidden, or s at the top
(declare tree->lists)

(declare followpath)



(register-handler
 ::state-from-conn
 (fn  state-from-conn [db]
   (let [conn (:ds db)
         root (posh/pull conn '[*] (:root-eid db 0))
         path (:path db :node/children)
         vdepth (:visible-depth db 4)
         errthin (posh/pull conn `[:db/id {~path ~vdepth}] (:db/id @root))
         newdb (merge db    
                     {:root @root
                      :depth (:depth db 0)
                      :root-list (followpath [path ALL] :db/id vdepth @errthin)})]
             
     (if (= vdepth (count (:cursor db)))
       newdb
       (assoc newdb :cursor (vec (for [i (range vdepth)] 0))))
     )))


(dispatch [::state-from-conn])






(defpathedfn repeat-path [walk-path end-path i]
  (if (= 0 i)
    end-path
    [walk-path (repeat-path walk-path end-path (dec i))]))



(defn followpath [walkpath endpath depth tree]
  (vec  (for [i (range depth)] 
        (vec (set (select (repeat-path walkpath endpath i) tree))))))







(register-sub
 ::key
 (fn [db [_ k]]
   (reaction (get @db k))))


(register-sub
 ::root-list
 (fn [db _ [conn]]
   (reaction (:root-list @db))))


(register-handler
 ::assoc
 (fn [db [_ k v]]
   (assoc db k v)))











(register-handler
 ::set-root
 (fn [db [_ eid]]
   (dispatch [::state-from-conn])
   (assoc db :root-eid eid)))









(register-handler 
 ::inc-depth
 (fn [db]
   (do (js/console.log (pr-str (count (:root-list db))))
       (if (= (inc (:depth db)) (count (:root-list db)))
         (assoc db :depth 0)
         (update db :depth inc)))))


(register-handler 
 ::dec-depth
 (fn [db]
   (if (= (:depth db) 0) 
     (assoc db :depth (dec (count (:root-list db))))
     (update db :depth dec)
     )))




(register-handler
 ::move-cursor
 (fn [db [_ cell-depth newval]]
   (->> (setval [:cursor (keypath cell-depth)] newval db)
        (setval [:depth] cell-depth))))




(register-handler
 ::dec-cursor
 (fn [db]
   (let [d (:depth db) ]
     (transform [:cursor (keypath d)]
                (fn [v] 
                  (if (= 0 v)
                    (dec (-> db
                             :root-list
                             (nth d)
                             count))
                    (dec v)))
                db))))



(register-handler
 ::inc-cursor
 (fn [db]
   (let [d (:depth db) ]
     (transform [:cursor (keypath d)]
                (fn [v] 
                  (if (= (inc v) (-> db
                                     :root-list
                                     (nth d)
                                     count))
                    0
                    (inc v)))
                db))))



(defn nav-keys [{:keys [left right down up]}]
  (key/bind! right ::right #(dispatch [::inc-depth]))
  (key/bind! left ::left #(dispatch [::dec-depth]))
  (key/bind! up ::up #(dispatch [::dec-cursor]))
  (key/bind! down ::down  #(dispatch [::inc-cursor]))
  #_(key/bind! "ctrl-a" ::aaa  #(do
                                (js/console.log "saved")
                                (save "a" @app-db)))
  
  #_(key/bind! "ctrl-b" ::bold  #(do
                              (js/console.log "heyyb")))
)






#_(defcard-rg v4*
  [gridview conn]
  cc
  {:inspect-data true})



(defn active-ent [db]
  (let [depth (:depth db)
        list (:root-list db)
        cursor (:cursor db)]
    (-> list
        (nth depth)
        (nth (nth cursor depth)))))
    

(register-sub
 ::active-entity3
 (fn [db]
   (let [depth (reaction (:depth @db))
         list (reaction (:root-list @db))
         cursor (reaction (:cursor @db))]
     (reaction (-> @list
                   (nth @depth)
                   (nth (nth @cursor @depth)))))))


(register-sub 
 ::active-entity
 (fn [db]
   (reaction (active-ent @db))))



(register-sub
 ::active-entity2
 (fn [db]
   (reaction (::active-entity @db))))



(defn add-child [db]
   (let [conn (:ds db)
         current (subscribe [::active-entity])]
     (do
       (d/transact! conn [{:db/id -1
                           :node/type :text
                          :node/text "New Node"}
                         [:db/add @current :node/children -1]])
       (dispatch [::state-from-conn])) 
     db))



(defn remove-node [conn eid]
  (d/transact! conn [[:db.fn/retractEntity eid]]))


(register-handler
 ::remove-node
 (fn [db]
   (let [conn (:ds db)
         eid (subscribe [::active-entity])]
     (if (= @eid 0)
       (js/alert "Can't remove the root node")
       (do
         (remove-node conn @eid)
         (dispatch [::dec-depth])
         (dispatch [:state-from-conn])))
     db)))



(register-handler
 ::add-child
 add-child)



(register-sub
 ::edit-mode
 (fn [db]
   (reaction (::editing @db false))))



(register-handler
 ::edit-mode
 (fn [db]
   (let [conn (:ds db)
         e (subscribe [::active-entity])
         text (posh/pull conn '[*] @e)]
      (key/unbind-all!)
      (dispatch [::assoc ::editing true])
      (dispatch [::assoc ::text (:node/text @text)])
;      (js/alert (str "Editing " @e ))
      (key/bind! "enter" ::edit #(dispatch [::edit-text conn]))
      (key/bind! "esc" ::normal #(dispatch [::nav-mode conn]))
   db)))



 (register-handler
 ::edit-text
 (fn [db]
   (let [conn (:ds db)
         current (subscribe [::active-entity])
         text (subscribe [::key ::text])]
     (d/transact! conn [[:db/add @current :node/text @text]])
     (dispatch [::nav-mode conn])
     db)))    



(defn node [i e] 
  (let [catom (subscribe [::cursor])
        editing? (subscribe [::edit-mode])
        text  (subscribe [::key ::text])
        conn (subscribe [::conn])
        node (posh/pull @conn '[*] e)]
    
    (fn [i e]
      (if (= @catom i)
        (dispatch [::assoc ::active-entity e]))
      (if (and  (= @catom i) @editing?)
        [:div
         [:input
          {:value @text
           :style {:width "100%"}
           :auto-focus "auto-focus"
           :on-change #(dispatch [::assoc ::text (-> % .-target .-value)])}]]
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







(register-handler
 ::nav-mode
 (fn [db]
   (dispatch [::assoc ::editing false])
   (key/unbind-all!)
   (nav-keys {:left "h" :right "l" :down "j" :up "k" })
   (key/bind! "i" ::add-child  #(dispatch [::add-child]))
   (key/bind! "x" ::remove-node  #(dispatch [::remove-node]))
   (key/bind! "r" ::root #(dispatch [::set-root-to-current]))
   (key/bind!  "e" ::edit #(dispatch [::edit-mode]))
   #_(save-load  {:save "s" :load "w"})
   db))





(declare text-node)

(defmulti cell-views (fn [node] (:node/type node :blank)))

(defmethod cell-views  :text [node] [text-node (:db/id node)])
(defmethod cell-views  :root [node] [:div (pr-str node)])
(defmethod cell-views  :blank [] [:div "blank?"])



(s/def ::atom
  (partial instance? cljs.core/Atom))




(s/fdef cell-views :args (s/cat ::atom map?))

(s/instrument #'cell-views)






(defn cell-view2 [column-depth cell-index eid]
 (let [conn (subscribe [::conn])
       active (subscribe [::active-entity])
       e (posh/pull @conn '[*] eid)] 
   
   (fn [column-depth cell-index eid]
     [:div {:style 
            {:padding "5px"}}
      (cell-views @e)
      [:button {:style 
           {:background-color 
            (if (=  eid @active)
              "green"
              "white")}
           :on-click #(dispatch [::move-cursor column-depth cell-index])}
       eid]
      [:a {:on-click #(do 
                        (dispatch [::assoc :root-eid eid])
                        (dispatch [::state-from-conn]))} :focus]])))




(:root-list
@app-db
)


(register-handler
 ::set-root-to-current
 (fn [db]
   (let [conn (:ds db)
         ae (active-ent db)]
     (dispatch [::set-root ae])
     db)))



(defn column2 [column-index column-val]
  (let [depth (subscribe [::key :depth])]
    (fn [column-index column-val]
      [:div
       {:style {:flex-grow 1
                :border (if (= column-index @depth)
                          "2px solid red"
                          "1px solid black")}}
       (doall (for [[r cell] (map-indexed vector column-val)]
                ^{:key cell}[cell-view2 column-index r cell]))])))




(defn gridview2 []
  (let [depth (subscribe [::key :depth])
        list (subscribe [::root-list])]
    (dispatch [::nav-mode])
    (fn []
      [:div {:style {:display "flex"
                     :flex-direction "row"}}
       [:button {:on-click #(dispatch [::set-root 0])} 0]
       (for [[d c] (map-indexed vector @list)]
          ^{:key (str d c)} [column2 d c])])))




(defcard-rg grid2
  [gridview2]
  cc)



(defn text-node [e] 
  (let [conn (subscribe [::conn])
        active-entity (subscribe [::active-entity])
        editing? (subscribe [::edit-mode])
        text  (subscribe [::key ::text])
        node (posh/pull @conn '[*] e)]
    
    (fn [e]
      [:div
       [:h1  (str "a"  (pr-str e))]
       (if (= @active-entity e)
         (dispatch [::assoc ::active-entity e]))]
      (if (and  (= @active-entity e) @editing?)
        [:div
         [:input
          {:value @text
           :style {:width "100%"}
           :auto-focus "auto-focus"
           :on-change #(dispatch [::assoc ::text (-> % .-target .-value)])}]]
        [:div        
         {:style 
          {:display "flex"
           :align-items "center"
         ;  :flex-direction "row"
           :border "1px solid grey"
           :justify-content "space-between"
           :background-color (if (= @active-entity e)
                               "green"
                               "white")}
          ;:on-click #(dispatch [::move-cursor e])
          }
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defcard mybox
"```
.box18 {
  background-color: #444;
  color: #fff;
  border-radius: 5px;
  padding: 20px;
  font-size: 150%;
}

.box18:nth-child(even) {
  background-color: #ccc;
  color: #000;
}

```" )




(defcard-rg grid4
[:div.wrapper18
 [:div {:style
        {:grid-column "2 / 4"
         :grid-row  "1 / 2" 
         :padding 20
         :background-color "#444"}} :a]
 [:div {:style
        {:grid-column "2 / 4"
         :grid-row  "4 / 50" 
         :padding 20
         :background-color "#ccc"}} :a]])





































(defn reset-test []
  (let  [conn (subscribe [::conn])
         es (posh/q  @conn '[:find ?e 
                             :where [?e ?a ?v]])]
    (fn [conn]
      [:div.wrapper18
       (for [e @es]
         [:div.box18 (pr-str e)])])))


(defcard-rg crazy2
  "ghhh"
  [reset-test conn])




#_(defn transact-and-store! 
([txt]  (transact-and-store! conn txt))
([con txn]
  (->> txn (d/transact! con) :db-after !>ls)))







(defcard-rg grid18
"
```.wrapper18 {
    width: 600px;
    display: grid;
    grid-gap: 10px;
    grid-template-columns: repeat(6, 100px);
}

```
"



  [:div.wrapper18
   (for [a (range 20)]
     [:div.box18 a]
       )])




(defcard-rg grid19
"
```
.wrapper19 {
    width: 600px;
    display: grid;
    grid-gap: 10px;
    grid-template-columns: repeat(6, 100px);
    grid-template-rows: 100px 100p")



