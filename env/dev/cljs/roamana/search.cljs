(ns roamana.search
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
            [clojure.set   :as set]
            [cljs.spec  :as s]
            [roamana.zz :refer [cursify]]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select select-one
                                    setval defnav 
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







;;root is hidden, or s at the top
(declare tree->lists)

(declare followpath)









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






























 
 
 
 


;;  todo, change active ent to be a db lookup in ds

(defn active-ent [db]
  (let [depth (:depth db)
        list (:root-list db)
        cursor (:cursor db)]
    (-> list
        (nth depth)
        (nth (nth cursor depth)))))
    




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
   (let [conn (:ds conn)
         eid (subscribe [::active-entity])]
     (remove-node conn @eid)
     (dispatch [::state-from-conn conn])
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











(register-handler
 ::nav-mode
 (fn [db]
   (dispatch [::assoc ::editing false])
   (key/unbind-all!)
   (key/bind! "i" ::add-child  #(dispatch [::add-child]))
   (key/bind! "x" ::remove-node  #(dispatch [::remove-node]))
   (key/bind! "r" ::root #(dispatch [::set-root-to-current]))
   (key/bind!  "e" ::edit #(dispatch [::edit-mode]))
   #_(save-load  {:save "s" :load "w"})
   db))







(defmulti cell-views (fn [node] (:node/type node :blank)))

(defmethod cell-views  :text [node] [:div (:db/id node)])
(defmethod cell-views  :root [node] [:div (pr-str node)])
(defmethod cell-views  :blank [] [:div "blank?"])

























(register-handler
 ::set-root-to-current
 (fn [db]
   (let [conn (:ds db)
         ae (active-ent db)]
     (dispatch [::set-root ae])
     db)))
















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
(def lorem 

"lorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnoj
lorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem [[embed]] ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnoj"
)


(defn area 
  ([name]  {:style {:grid-area name}}))



(register-sub
 ::search
 (fn [db]
   (reaction (::search @db ""))))


(defn search []
  (let [s (subscribe [::search])]
    (fn []
      [:div.search
       [:input#search
        {:value @s
         :on-change  #(do
                        (dispatch [::assoc ::depth 0])
                        (dispatch [::assoc ::search (->
                                                       %
                                                       .-target
                                                       .-value)]))}]
       [:button :a]
       [:button :b]])))



(register-sub 
 ::text-nodes
 (fn [db]
   (let [conn (:ds @db)]
     (posh/q conn '[:find ?e ?text
                    :where [?e :node/text ?text]]))))



(re-find  #".*a.*b.*c.*" "abadb")

(defn ido-regex [s] 
  (js/RegExp. 
   (apply str  
          (concat 
           (interleave 
            (repeat ".*") 
            (map (partial apply str)
                 (partition 3                            
                            (interleave (repeat "(")
                                        (clojure.string/split s "")
                                        (repeat ")")))))  ".*") ) "i"))

(map (partial  apply str) (partition 3 (interleave (repeat "(")
                                 (clojure.string/split "abc" "")
                                 (repeat ")"))))



#_(register-handler
 ::state-from-conn
 (fn  state-from-conn [db]
   (let [conn (:ds db)
         root (posh/pull conn '[*] (:root-eid db 0))
         path (:path db :node/children)
         vdepth (:visible-depth db 4)
         errthin (posh/pull conn `[:db/id {~path ~vdepth}] (:db/id @root))]
     (merge db    
            {:root @root
             :depth 0
             :cursor (vec (for [i (range vdepth)] 0))
             :root-list (followpath [path ALL] :db/id vdepth @errthin)}))))


#_(dispatch [::state-from-conn])

(register-handler 
 ::init-state
 (fn [db]
   (merge db {::depth 0
              ::search  ""
              ::ds conn})))

(dispatch [::init-state])

(register-sub
 ::depth
 (fn [db]
   (reaction (::depth @db))))


(register-handler 
 ::down
 (fn [db]
   (if (> 10 (::depth db))
     (update db ::depth inc)
      (assoc db ::depth 0))))


(register-handler
 ::up
 (fn [db]
   (if (< 0 (::depth db))
     (update db ::depth dec)
     db)))



(key/bind! "ctrl-j" ::down  #(dispatch [::down]))
(key/bind! "ctrl-k" ::down  #(dispatch [::up]))


(register-sub
 ::r
 (fn [_ _ [results search]]
   (reaction (filter (fn [[_ t]]
                       (re-find
                        (ido-regex search)
                        t)) results))))


(register-sub
 ::results
 (fn [_]
   (let [texts (subscribe [::text-nodes])
         search (subscribe [::search])]
     (reaction (filter (fn [[_ t]]
                         (re-find
                          (ido-regex @search)
                          t)) @texts)))))


(subscribe [::results])


#_(subscribe [::r] [(subscribe [::text-nodes])
                 (subscribe [::search])])


(defn outline []
  (let [results (subscribe [::text-nodes])
        depth  (subscribe [::depth])
        search  (subscribe [::search])
        tesst (subscribe [::results])
        r (subscribe [::r] [results search])]
    (fn []
        [:div.outline
         (doall (for [[pos [id text]] (map-indexed vector @tesst)
                      :while (> 20 pos)]
           [:div.node
            (if (= pos @depth)
              {:class "active"})
            (pr-str text @depth
                    pos)]))
  #_(for [r  @results]
      [:div.box r])])))




(defn resize-area []
  (let [note (.getElementById js/document "note")]
    (.-scrollHeight note)))



(defn grid-frame []
  (fn []
    [:div.grid-frame
     [search]
     [outline]
     [:div.note
      [:textarea#note]]
     [:div (area "footer")
      :aa]
     ]))

(defcard-rg frame
  [grid-frame])


(.getElementById js/document "search")





(defn move-focus [id e]
  (let [el (.getElementById js/document id)]
    (.preventDefault e)
    (js/console.log e)
    (.focus el)))




(key/bind! "ctrl-l" ::focus-search #(move-focus "search" %))
(key/bind! "ctrl-n" ::focus-search #(move-focus "note" %))

(key/bind! "tab" ::focus-search #(move-focus "note" %))

