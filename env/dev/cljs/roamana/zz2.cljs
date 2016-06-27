(ns roamana.zz2
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      if-path END cond-path
                                      must pred keypath
                                      collect-one comp-paths] :as sp]
            [roamana.logger :refer [all-ents]]
            [roamana.views :refer [main-view logmap todo-create] :as views]
            [reagent.session :as session]
            [keybind.core :as key]
            [roamana.zz :refer [cursify]]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select setval defnav transform declarepath providepath]]
   [reagent.ratom :refer [reaction]]
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(re-frame.utils/set-loggers! {:warn #(js/console.log "")})



(def schema {:node/children {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}})


(defonce conn (d/create-conn schema))
(posh! conn)

(def cc (cursify conn))



(d/transact! conn [{:db/id 0 :node/type :root :node/children #{1 2}}
                   {:db/id 1 :node/type :text :node/text "Main 1"  
                    :node/children #{3}} 
                   {:db/id 2 :node/type :text :node/text "Main 2"
                    :node/children #{3}} 
                   {:db/id 3 :node/type :text :node/text "Main 1 &2 : Child 1"
                    :node/children #{4}} 
                   {:db/id 4 :node/type :text :node/text "Child 1: Grandkid 1"
                    :node/children #{5}}
                   {:db/id 5 :node/type :text :node/text "Grandkid 1 : Great 1"} ])


(register-sub
 ::all
 (fn [db]
   (reaction @db)))


(register-sub
 ::all-ents
 (fn [db [_ conn]]
   (posh/q conn '[:find (pull ?e [*])
                    :where  [?e]])))


(defn ents [conn]
  (let [es (subscribe [::all-ents conn])
        all (subscribe [::all])]
  (fn []
    [:div 
     [:div (pr-str @all)]])))



;;root is hidden, or is at the top

(register-handler
 ::state-from-conn
 (fn [db [_ conn]]
   (let [root (posh/pull conn '[*] (:root-eid db 0))
         errthin (posh/pull conn '[:db/id {:node/children 3}] 0)]
     (merge db    
            {:root @root
             :depth 0
             :cursor [[0]]
             :errthin @errthin}))))




(defcard-rg errthing
  [:div
   [:button {:on-click #(dispatch [::state-from-conn conn])} "state"]
   [ents conn]]
)






(register-handler
 ::move-cursor
 (fn [db [_ pos]]
   (assoc db ::cursor pos)))



(register-sub
 ::key
 (fn [db [_ k]]
   (reaction (get @db k))))


(register-handler
 ::assoc
 (fn [db [_ k v]]
   (assoc db k v)))



(register-sub
 ::active-entity 
 (fn [db]
   (reaction (::active-entity @db 0))))




(defn add-child [db [_ conn]]
   (let [current (subscribe [::active-entity])]
     (d/transact! conn [{:db/id -1
                         :node/text "New Node"}
                        [:db/add @current :node/children -1]]) 
     db))



(register-handler
 ::add-child
 add-child)


(register-sub
 ::cursor
 (fn [db]
   (reaction (::cursor @db 0))))

(register-sub
 ::edit-mode
 (fn [db]
   (reaction (::editing @db false))))



(register-handler
 ::edit-mode
 (fn [db [_ conn]]
   (let [e (subscribe [::active-entity])
         text (posh/pull conn '[*] @e)]
      (key/unbind-all!)
      (dispatch [::assoc ::editing true])
      (dispatch [::assoc ::text (:node/text @text)])
      #_(js/alert (str "Editing " @e ))
      (key/bind! "enter" ::edit #(dispatch [::edit-text conn]))
      (key/bind! "esc" ::normal #(dispatch [::nav-mode conn]))
   db)))



 (register-handler
 ::edit-text
 (fn [db [_ conn]]
   (let [current (subscribe [::active-entity])
         text (subscribe [::key ::text])]
     (d/transact! conn [[:db/add @current :node/text @text]])
     (dispatch [::nav-mode conn])
     db)))    



(register-handler
 ::nav-mode
 (fn [db [_ conn]]
   (let [cursor (subscribe [::cursor])]
     (dispatch [::assoc ::editing false])
     (key/unbind-all!)
     (key/bind! "j" ::up      #(dispatch [::move-cursor (inc @cursor)]))
     (key/bind!  "c" ::cursor #(js/alert @cursor))
     (key/bind!  "e" ::edit #(dispatch [::edit-mode conn]))
     (key/bind! "k" ::down    #(dispatch [::move-cursor (dec @cursor)]))
     (key/bind! "i" ::child  #(dispatch [::add-child conn]))
     (key/bind! "n" ::new     #(d/transact! conn [{:db/id -1 :node/text "untitled"}]))
     db)))





(defn node2 [i e conn] 
  (let [catom (subscribe [::cursor])
        editing? (subscribe [::edit-mode])
        text  (subscribe [::key ::text])
        node (posh/pull conn '[*] e)]
    
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
          :on-click #(dispatch [::move-cursor i])}
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







(defn node3 [i e conn] 
  (let [catom (subscribe [::cursor])
        editing? (subscribe [::edit-mode])
        text  (subscribe [::key ::text])
        node (posh/pull conn '[*] e)]
    
    (fn [i e]
        [:div        
         {:style 
          {:border "1px solid grey"
           :background-color (if (= @catom i)
                               "green"
                               "blue")}}
         (:node/text @node)
         (if-let [children (:node/children @node)]
           (count children))])))






(register-sub
 ::children
 (fn [_ [_ conn] [ae]]
     (posh/q conn '[:find (pull ?e [*])
                    :in $ ?parent
                    :where  [?parent :node/children ?e]]
             ae)))


(defn children1 [conn]
  (let [active (subscribe [::active-entity]) 
        children (subscribe [::children conn][active])]
    (fn []
      [:div
       {:style {:width "50%"
                :display "flex"
                :justify-content "center"
                :flex-direction "column"}}
       (for [[i [e]] (map-indexed vector @children)]
           ^{:key e}[:div (pr-str e)])])))




(defn selects3 [conn]
  (let [e (subscribe [::active-entity])
        db (subscribe [::all])
        es (posh/q conn '[:find ?p
                          :where 
                          [?p :node/text _]])
        kids (posh/q conn '[:find ?p
                            :where [_ :node/children ?p]])]
    (fn []
      (let [roots (clojure.set/difference @es @kids)]
        [:div (pr-str @db)
         [:div    
          {:style {:display "flex"
                   :flex-direction "row"}}
          [:button {:on-click #(dispatch [::nav-mode conn])} "SETUP"]
          [:div 
           {:style {:width "50%"
                    :display "flex"
                    :flex-direction "column"}}
           (for [[i [e]] (map-indexed vector (sort roots))]
             ^{:key e}[node2 i e conn])]
          [children1 conn]
          ]]))))
        

(register-sub 
 ::all
 (fn [db]
   (reaction @db)))




(defcard-rg Editables
 [selects3 conn])



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



#_(swap! cursor-vec (fn [m] (transform [:cursor #(nth % 3) last] inc m)))



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
                            (dec count)
                            (dec value)))) db)))


#_(def inc-cursor (partial adjust-cursor inc))
#_(def dec-cursor (partial adjust-cursor dec))




(declare inc-depth)


(defn vec-keys [atom]
  (key/unbind-all!)
  (key/bind! "l" ::left #(swap! atom inc-depth))
  (key/bind! "h" ::right #(swap! atom update :depth dec))
  (key/bind! "j" ::down  #(swap! atom dec-cursor))
  (key/bind! "k" ::up  #(swap! atom inc-cursor))
)


(defn vecview [atom]
  (fn []
    (let [cursor (:cursor @atom)
          depth (:depth @atom)]
      [:div {:style {:display "flex"
                     :flex-direction "row"}}
       (for [[d column] (map-indexed vector (:lists @atom))]
         ^{:key (str d column)}
         [:div
          {:style {:flex-grow 1
                   :border (if (= d depth)
                             "2px solid red"
                             "1px solid black")}}
          (for [[r row] (map-indexed vector column)]
            ^{:key (str d row column)}[:div 
                                       (if (and 
                                            (= r (nth cursor d)))
                                         [:h2 (pr-str (nth cursor d))]
                                         (pr-str row))])])])))


(map-indexed vector [:a :b :c])

(defcard-rg cursortes*t
  "hey"
  [vecview cursor-vec]
  cursor-vec
  {:inspect-data true})


;;;;;;;;;;;;;;;;;;;;;;;;;;




(defonce conn3 (d/create-conn schema))
(posh! conn3)


(def cc3 (cursify conn3))


(declare cursor-vec2)


(defn current-cursor [db]
  (let [{:keys [depth cursor lists]} @db
        current-position (nth cursor depth)
        e (-> lists
                            (nth depth)
                            (nth current-position))]
    e))





(defn inc-cursor2 [db]
  (let [d (:depth db)
        l (:lists db)
        count (-> (nth l d)
                  count)]
    (js/console.log [d l count])
    (transform [:cursor (sp/srange d (inc d))] 
               (partial map 
                        (fn [value]
                          (if (= count (inc value))
                            0
                            (inc value)))) db)))


(defn dec-cursor2 [db]
  (let [d (:depth db)
        l (:lists db)
        c (-> (nth l d)
                  count)]
    (do (js/console.log [d (nth l d) c])
        (transform [:cursor (sp/srange d (inc d))] 
                   (partial map 
                            (fn [value]
                              (do
                                (js/console.log "value" value)
                                (if (= -1 (dec value))
                                  (dec c)
                                  (dec value))))) db))))




(defn inc-depth [atom]
  (let [d (:depth atom)
        list-count  (count (:lists atom))]
    (if (= d (dec list-count))
      (assoc atom :depth 0)
      (update atom :depth inc))))


(declare add-nodes)

(defn ds-vec-keys [atom conn]
  (key/unbind-all!)
  (key/bind! "l" ::left #(swap! atom inc-depth))
  (key/bind! "h" ::right #(swap! atom update :depth dec))
  (key/bind! "k" ::down  #(swap! atom dec-cursor2))
  (key/bind! "j" ::up  #(swap! atom inc-cursor2))
  (key/bind! "y" ::replace  #(swap! atom (partial add-nodes conn)))
  (key/bind! "n" ::new #(d/transact! conn [{:db/id -1 :node/text "boo"}])))


(defn add-nodes [conn db]
  (if-let [nodes (posh/q conn '[:find (pull ?e [*])
                             :where [?e :node/text]])]
    (-> (update db :lists conj @nodes)
        (update :cursor conj 0))
    db))




(def cursor-vec2 (atom {:depth 0
                        :cursor [0 0 0 0]
                        :lists [[:a :b :c :d :e]
                                [:aa :bb :cc :dd]
                                [1 2 3 4 5]
                                [:z :y :x]]}))

(defn ds-vecview [catom conn]
  (let [nodes (posh/q conn '[:find (pull ?e [*])
                             :where [?e :node/text]])]
    (fn []
      [:div
       [:button {:on-click #(ds-vec-keys catom conn)} "HOTKESY"]
       [:h1 (pr-str (current-cursor catom))]])))





(defcard-rg cursorvec3*
  [vecview cursor-vec2]
  cursor-vec2
  {:inspect-data true})

(defcard-rg cursor-ds-test
  "hey"
  [ds-vecview cursor-vec2 conn3]
  cc3
  {:inspect-data true})



(def cursor-vec3 (atom {:depth 0
                        :cursor [0]
                        :lists [[:a :b]]}))


(transform [#_
            (sp/if-path (sp/srange 1 2)) LAST] inc [0])



(defnav nth-elt [n] 
     (select* [this structure next-fn] (next-fn (nth structure n)))
     (transform* [this structure next-fn] 
       (let [structurev (vec structure) 
             ret (next-fn (nth structure n))] 
         (if (vector? structure) 
           (assoc structurev n ret)
           (concat (take n structure) (list ret) (drop (inc n) structure))))))




(defnav nth-elt2 [n] 
     (select* [this structure next-fn] (next-fn (nth structure n)))
     (transform* [this structure next-fn] 
       (let [structurev (vec structure) 
             ret (next-fn (nth structure n))] 
         (if (vector? structure) 
           (if (= n (count structure))
             (conj structure 0)
             (assoc structurev n ret))
           (concat (take n structure) (list ret) (drop (inc n) structure))))))



#_(setval [(if-path (must 3) (keypath 3) END)] :newd
            [:a :b :c :dd])


#_(transform [sp/ATOM :lists]
           (fn [l]
             (assoc l 2 [1 2 3])) cursor-vec3)


(defn switch-nodes [db conn]
  (let [e (current-cursor db)
        all-es (posh/q conn '[:find ?e 
                                :where [?e]])
        d (:depth @db)]
    (transform [sp/ATOM :lists]
               (fn [l]
                 (assoc l d (map first @all-es))) db)))



(defn add-kid [db conn]
  (let [e (current-cursor db)
        d (:depth @db)]
    (d/transact! conn [[:db/add e :node/children -1]
                       {:db/id -1 :node/text "newkid"}])))



(defn get-kids [db conn]
  (let [e (current-cursor db)
        all-kids (posh/q conn '[:find ?e 
                                :where [_ :node/children ?e]])
        d (:depth @db)]
    (transform [:lists]
               (fn [l]
                 (assoc l (inc d) (select [ALL ALL] @all-kids))) @db)))




;(switch-nodes cursor-vec2 conn3)

(defn ds-vec-keys2 [atom conn]
  (key/unbind-all!)
  (key/bind! "l" ::left #(swap! atom inc-depth))
  (key/bind! "h" ::right #(swap! atom update :depth dec))
  (key/bind! "k" ::down  #(swap! atom dec-cursor2))
  (key/bind! "j" ::up  #(swap! atom inc-cursor2))
  (key/bind! "c"  ::child  #(add-kid atom conn))
  (key/bind! "ctrl-c"  ::seechild  #(reset! atom (get-kids atom conn)))
  (key/bind! "y" ::replace  #(switch-nodes atom conn))
  (key/bind! "n" ::new #(d/transact! conn [{:db/id -1 :node/text "boo"}])))





#_(defn node4 [conn eid]
  (let [e (posh/pull conn '[*] eid)]
    (fn []
      [:div (pr-str @e)])))

(defn vecview2 [atom]
  (fn []
    (let [cursor (:cursor @atom)
          depth (:depth @atom)]
      [:div {:style {:display "flex"
                     :flex-direction "row"}}
       (for [[d column] (map-indexed vector (:lists @atom))]
         ^{:key (str d column)}
         [:div
          {:style {:flex-grow 1
                   :border (if (= d depth)
                             "2px solid red"
                             "1px solid black")}}
          (for [[r row] (map-indexed vector column)]
            ^{:key (str d row column)}[:div 
                                       (if (and 
                                            (= r (nth cursor d)))
                                         [:h1 (pr-str row)]
                                         (pr-str row))])])])))




(defn ds-vecview2 [catom conn]
  (let [nodes (posh/q conn '[:find (pull ?e [*])
                             :where [?e :node/text]])]
    (fn []
      [:div
       (pr-str @catom)
       [:button {:on-click #(ds-vec-keys2 catom conn)} "HOTKEY"]
       [:h1 (pr-str (current-cursor catom))]
       [vecview2 catom]])))



(defcard-rg cursor-ds-test2
  "hey"
  [ds-vecview2 cursor-vec3 conn3]
  cursor-vec3
  {:inspect-data true})

