(ns roamana.query
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



(register-sub
 ::key
 (fn [db [_ k]]
   (reaction (get @db k))))

(register-handler
 ::assoc
 (fn [db [_ k v]]
   (assoc db k v)))










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


;

;








#_(deftest searchtests
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


(register-sub
 ::active-entity
 (fn [] 
   (let
       [d  (subscribe [::depth]) 
        r  (subscribe [::results1])]
     (reaction  (get (select [ALL sp/FIRST] @r) @d 0)))))



(register-sub
 ::pull
 (fn [db _ [eid]]
   (let [conn (:ds @db)]
     (posh/pull conn '[*] eid))))


(register-handler
 ::transact
 (fn [db [_ transaction & {:keys [ds-id] :or {ds-id :ds}}]]
   (let [conn (get db ds-id)]
     (d/transact! conn transaction)
     db)))









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
        dv (subscribe [::pull][d])]
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
       
       ])))

(defcard-rg grid
  [grid-frame])







;;;;;;;;;;;;;;;;;;;multisubs




(register-sub
 ::intersection
 (fn [_ _ sets]
   (reaction (apply set/intersection sets))))

(register-sub
 ::union
 (fn [_ _ sets]
   (reaction (apply set/union sets))))


(register-sub
 ::difference
 (fn [_ _ sets]
   (reaction (apply set/difference sets))))


#_(posh/q conn '[:find ?e
                     :in $ ?p1 ?p2
                     :where 
                     [?p1 :node/children ?e]
                     [?p2 :node/children ?e]] a b)


(register-sub
 :flat/attr
 (fn [db [_ attr eid] [conn]]
     (posh/q conn '[:find ?ret
                    :in $ ?attr ?id
                    :where 
                    [?id ?attr ?ret]]
             attr
             eid)))



#_(deftest substest
  (testing "errthing"
    (let [schema {:node/children {:db/valueType :db.type/ref
                                  :db/cardinality :db.cardinality/many}}
          mconn (d/create-conn schema)]
      
      (is (= 1 1))
      (do  
        (posh! mconn)
        (d/transact! mconn [{:db/id 1 
                            :node/text "Set A"
                            :node/children #{2 3 4 5}}
                           {:db/id 2 
                            :node/text "Node A"
                            :node/children #{}}
                           {:db/id 3
                            :node/text "Node 3"
                            :node/children #{}}
                           {:db/id 4
                            :node/text "Node 4"
                            :node/children #{}}
                           {:db/id 5 
                            :node/text "Node 5"
                            :node/children #{}}
                           {:db/id 6
                            :node/text "Node 6"
                            :node/children #{}}
                           {:db/id 7 
                            :node/text "Node 7"
                            :node/children #{}}
                           {:db/id 8
                            :node/text "Set B"
                            :node/children #{4 5 6 7}}
                           ])
        
        
        (let [b  (posh/q mconn '[:find ?e
                                         :in $ ?s
                                         :where [?e :node/text ?s]]
                                  "Set B")
              a (posh/q mconn '[:find ?e
                                        :in $ ?s
                                        :where [?e :node/text ?s]]
                                 "Set A")]
          (testing "fixtures"
            (is (= @a #{[1]}))
            (is (ds-db? @mconn) )
            
            ))
          #_(d/transact! conn [{:db/id 3  :node/type :intersection
                                :node/children #{50 51}}])
          
          #_(->>
             @(posh/pull conn  '[:node/type :node/text {:node/children 2}] 52)
             (select [:node/children ALL])
             (map  #(select [:node/children ALL :db/id] %))
             (map set)
             (apply set/intersection )
             cljs.pprint/pprint
             
             )))))


(register-sub
 :dynamic/attr
 (fn [db [_ attr] [eid]]
   (let [conn (:ds @db)]
     (posh/q conn '[:find ?ret
                    :in $ ?attr ?id
                    :where 
                    [?id ?attr ?ret]]
             attr
             eid))))

#_(subscribe [::intersection] 
           [(subscribe [:dynamic/attr :node/children] [(reaction 50)])
            (subscribe [:dynamic/attr :node/children] [(reaction 51)])])



(defn outline-filter []
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


(defn grid-frame2 []
  (let [localstate (atom {})
        d (subscribe [::active-entity])
        dv (subscribe [::pull][d])]
    (fn []
      [:div {:style  {:display "grid"
                      :grid-template-columns "10px 2fr 2fr 10px"
                      :grid-template-areas 
                      "'.. search  search ..'
                       '.. outline filterview ..'"
                      
                      }}
       [search]
       [outline-filter]
       [:div.note
        [:textarea#note
         {:value (:node/body @dv "")
          :on-change #(dispatch [::transact [{:db/id @d :node/body 
                                              (-> % .-target .-value)

}]])}
         ]]
       ])))


(defcard-rg wa
  [grid-frame2])













(deftest setstest
  (testing "set subscribes"
    (let [seta  
          (reaction (set  (range 10)))
          setb 
          (reaction (set  (range 4 12)))]
      (is (= #{4 5 6 7 8 9}
             @(subscribe [::intersection] [seta setb])))
      (is (= #{0 1 2 3 4 5 6 7 8 9 10 11 }
             @(subscribe [::union] [seta setb])))
      (testing 
          "Difference takes the first set, and removes values in each subsequent set"
        (is (= #{0 1 2 3}
             @(subscribe [::difference] [seta setb])))
))))






