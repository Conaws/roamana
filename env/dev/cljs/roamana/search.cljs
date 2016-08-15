(ns roamana.search
  (:require [reagent.core :as r :refer [atom]]
            [reagent.ratom :refer [make-reaction]]
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
            [clojure.test.check.generators]
            [cljs.spec  :as s]
            [cljs.spec.impl.gen :as gen]
            [roamana.zz :refer [cursify]]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [cljs.test  :refer [testing is]]
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


(def ds-db? #(instance? datascript.db/DB %))

(def ratom? (partial instance? reagent.ratom/RAtom))

(def atom? (partial instance? cljs.core/Atom))

(s/def ::ds  ds-db?)

(s/def ::conn  #(s/valid? ::ds @%))



(s/def ::app-db (s/and 
                 ratom?
                 #(s/valid? ::ds @(:ds @%))))

#_(deftest cljs-test-integration1
  "## Here are some example tests"
  (testing "testing context 1"
    (is (= (+ 3 4 55555) 4) "This is the message arg to an 'is' test")
    (is (= (+ 1 0 0 0) 1) "This should work")
    (is (= 1 3))
    (is false)
    (is (throw "errors get an extra red line on the side")))
   "Top level strings are interpreted as markdown for inline documentation."
  (testing "testing context 2"
    (is (= (+ 1 0 0 0) 1))        
    (is (= (+ 3 4 55555) 4))
    (is false)
  (testing "nested context"
    (is (= (+ 1 0 0 0) 1))        
    (is (= (+ 3 4 55555) 4))
    (is false))))


(register-sub
 ::key
 (fn [db [_ k]]
   (reaction (get @db k))))



(register-handler
 ::assoc
 (fn [db [_ k v]]
   (assoc db k v)))

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


(declare move-focus)

(defn search []
  (let [s (subscribe [::search])
        d (subscribe [::depth])]
    (fn []
      [:div.search
       [:input#search
        {:value @s
         :on-key-press #(if (= (.-charCode %) 13)
                         (do
                           
                           #_(js/alert [@d @s])
                           (if (= 0 @d)
                             (dispatch [::transact [{:db/id -1
                                                     :node/text @s}]]))
                           (move-focus "note" %)))
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


(s/def ::eid integer?)
(s/def ::search-result (s/cat :id ::eid :name string?))
(s/def ::results (s/* 
                  (s/spec
                   ::search-result)))


;(s/exercise ::search-results)

;(s/explain ::search-result [1 "3"])

(s/explain ::results  [[1 "3"]])

(s/exercise (s/cat :k keyword? :ns (s/+ number?)) 5)




(deftest searchtests
  "## Here are some example tests"
  (testing "search"
    (let  [search (subscribe [::search])
           text-nodes (subscribe [::text-nodes])
           search-results (subscribe [::results1])
           depth  (subscribe [::depth])]
      (is (string? @search))
      (is (set?  @text-nodes))
      (is (s/valid?  ::results @search-results))
      (is (s/valid? integer? @(subscribe 
                               [::active-entity]
                                [depth search-results])))))
  
   "Top level strings are interpreted as markdown for inline documentation."
  (testing "testing context 2"
    (is (s/valid? ::app-db  app-db))
    (is (s/valid? ::ds  @(:ds @app-db)))

))


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
   (let [results @(subscribe [::results1])]
     (if (> (count results) (inc (::depth db)))
       (do (move-focus "note")
         (update db ::depth inc))))))




(deftest depthtest
  (testing "handlersubscriptions"
    (is (s/valid? integer?  (count @(subscribe [::results1])))))
)



(register-handler
 ::up
 (fn [db]
 ;  (js/alert "down")
   (if (< 0 (::depth db))
     (do (move-focus "note")
        (update db ::depth dec))
     (if (= 0 (::depth db)) 
       (do (move-focus "search")
         db)
       db))))




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
    (set! (.-scrollTop item) 50)))






(defn outline []
  (let [results  (subscribe [::results1])
        depth (subscribe [::depth])]
    (fn []
        [:div.outline
         (doall 
          (for 
              [[pos [id text]] 
               (map-indexed vector @results)]
           ^{:key id} [:div.node
               {:on-click
                #(fire-scroll %)
                :on-change #(js/console.log %)
                :class (if (= pos @depth)
                         "active"
                         "inactive")}
               (pr-str text)]))])))



#_(defcard-rg outline
  [outline]
  )


#_(defn resize-area []
  (let [note (.getElementById js/document "note")]
    (.-scrollHeight note)))


(get [[:a :b]] 0)


(register-sub
 ::active-entity
 (fn [] 
   (let
       [d  (subscribe [::depth]) 
        r  (subscribe [::results1])]
     (reaction  (get (select [ALL sp/FIRST] @r) @d 0)))))



(register-sub
 ::apull
 (fn [db _ [eid]]
   (let [conn (:ds @db)]
     (posh/pull conn '[*] eid))))







(register-handler
 ::transact
 (fn [db [_ transaction & {:keys [ds-id] :or {ds-id :ds}}]]
   (let [conn (get db ds-id)]
     (d/transact! conn transaction)
     db)))


(defn grid-frame0 []
  (let [d (subscribe [::active-entity])
        dv (subscribe [::apull][d])]
    (fn []
      [:div.grid-frame
       [search]
       [outline]
       [:div.note
        [:textarea#note
         {:value (:node/body @dv "")
          :on-change #(dispatch [::transact [{:db/id @d :node/body 
                                              (-> % .-target .-value)

}]])}
         ]]
       [:div (area "footer")
        :aa]
       ])))

(defcard-rg frame
  [grid-frame0])



(defn move-focus 
  ([id] (let [el (.getElementById js/document id)]
          (.focus el)))
  ([id e]
   (let [el (.getElementById js/document id)]
     (.preventDefault e)
     (.focus el))))




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



(defn Viewable [props]
  (r/create-class {:displayName "Viewable"
                   :component-did-mount
                   (fn [component]
                     (.scrollIntoViewIfNeeded (r/dom-node component)))
                   :reagent-render (fn [props]
                                     [:button props])}))

(defn outline1 []
  (let [results  (subscribe [::results1])
        depth (subscribe [::depth])]
    (fn []
        [:div.outline
         {:style {:display "grid" 
                  :grid-template-rows (str "repeat("
                                            (count @results)
                                            ", [row] 15%)")}}
         (doall 
          (for 
              [[pos [id text]] 
               (map-indexed vector @results)]
           (if (= pos @depth)
             [Viewable (pr-str text)]
             ^{:key id}[:div.node
                        {:on-click
                         #(do 
                            (move-focus "note")
                            (dispatch [::assoc ::depth pos]))
                         }
                        (pr-str text)])))])))


(defn grid-frame []
  (let [d (subscribe [::active-entity])
        dv (subscribe [::apull][d])]
    (fn []
      [:div.grid-frame
       [search]
       [outline1]
       [:div.note
        [:textarea#note
         {:value (:node/body @dv "")
          :on-change #(dispatch [::transact [{:db/id @d :node/body 
                                              (-> % .-target .-value)

}]])}
         ]]
       [:div (area "footer")
        :aa]
       ])))

(defcard-rg grid2
  [grid-frame])
