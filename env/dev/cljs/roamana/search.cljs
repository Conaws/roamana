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
                                      srange 
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


(def ds-db? #(instance? datascript.db/DB %))

(def ratom? (partial instance? reagent.ratom/RAtom))

(def atom? (partial instance? cljs.core/Atom))

(s/def ::ds  ds-db?)

(s/def ::conn  #(s/valid? ::ds @%))



(s/def ::app-db (s/and 
                 ratom?
                 #(s/valid? ::ds @%)))




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



(register-handler
 ::assoc
 (fn [db [_ k v]]
   (assoc db k v)))


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
   (reaction (::search @db))))




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


(s/fdef ido-regex 
        :args  (s/cat :regexer string?))



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




#_(defcard-rg search
  [search])

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




(register-sub
 ::depth
 (fn [db]
   (reaction (::depth @db))))








(register-handler 
 ::down
 (fn [db]
;   (js/alert "down")
   (if (> 20 (::depth db))
     (update db ::depth inc)
     (assoc db ::depth 0))))





(register-handler
 ::up
 (fn [db]
 ;  (js/alert "down")
   (if (< 0 (::depth db))
     (update db ::depth dec)
     db)))




(register-sub
 ::r
 (fn [_ _ [results search]]
   (reaction (filter (fn [[_ t]]
                       (re-find
                        (ido-regex search)
                        t)) results))))


(register-sub
 ::results1
 (fn [_]
   (let [texts (subscribe [::text-nodes])
         search (subscribe [::search])
         r  (subscribe [::r] [texts search])]
r)))






(s/fdef mynew
        :args  (s/cat :depth integer?
                      :max (s/? integer?))
        :ret  (s/cat  :start integer? :end integer?)
        :fn  #(and  (<= 0 (-> %  :ret :start))))
        


(defn mynew
  ([x]
   (let [y (- x 10)]
     [(if (<= 0 y)
        y
        0) 
      x]))
  ([z maxval]
   (let [x (min z maxval)
         y (- x 10)]
     [(if (<= 0 y)
        y
        0) 
      x])))


(s/instrument #'mynew)



(register-sub
 ::nahnah
 (fn [_ _ [total depth]]
   (let [d  (max 10 (inc depth))]
     (reaction (select-one (apply srange (mynew d (count total))) total)))))



(register-sub
 ::nah
 (fn [_ _ [total depth]]
   (let [d  (max 10 (inc depth))
         dd  (min d (count total))]
     (reaction (select-one (srange 0 dd) total)))))



(register-sub
 ::results3
 (fn [_]
   (let [total (subscribe [::results1])
         depth  (subscribe [::depth])]
     (subscribe [::nah] [total depth]))))




(defn fire-scroll [e]
  (let [item (-> e .-target)]
    (js/console.log  [item])
    (.scrollIntoViewIfNeeded item)
    (set! (.-scrollTop item) 5)))






(defn outline []
  (let [results  (subscribe [::results1])
        depth (subscribe [::depth])]
    (fn []
        [:div.outline
         (doall 
          (for 
              [[pos [id text]] 
               (map-indexed vector @results)]
           [:div.node
            {:on-click #(fire-scroll %)
             :class (if (= pos @depth)
                      "active"
                      "inactive")}
            (pr-str text)]))])))



(defcard-rg outline
  [outline])


#_(defn resize-area []
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

(defcard-rg framer
  [grid-frame])



(defn move-focus [id e]
  (let [el (.getElementById js/document id)]
    (.preventDefault e)
    (js/console.log e)
    (.focus el)))




(defn testa []
  (let [a (subscribe [::depth])]
    (js/console.log @a)))


(defn search-keys []
  (key/unbind-all!)
  (key/bind! "ctrl-l" ::focus-search #(move-focus "search" %))
  (key/bind! "ctrl-n" ::focus-search #(move-focus "note" %))
  (key/bind! "tab" ::focus-search #(move-focus "note" %))
  (key/bind! "ctrl-j" ::down  #(dispatch [::down]))
  (key/bind! "ctrl-a" ::down  #(testa))
  (key/bind! "ctrl-k" ::down  #(dispatch [::up])))



(defcard-rg  aaa
  [:button
   {:on-click #(search-keys)}
   :keys]
  )
(search-keys)
