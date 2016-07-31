(ns roamana.outline
  (:require [reagent.core :as r :refer [atom]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [instaparse.core  :as insta]
            [com.rpl.specter  :refer [ALL STAY FIRST
                                      MAP-VALS LAST
                                      stay-then-continue 
                                      if-path END cond-path
                                      srange must pred keypath
                                      collect-one comp-paths] :as sp]
            [keybind.core :as key]
            [clojure.test.check.generators]
            [roamana.search :as search]
            [roamana.zz :refer [cursify]]
            [roamana.query :as q]
            [cljs.spec  :as s]
            [roamana.util :as u]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as str]
            [cljs.spec.impl.gen :as gen]
            [devcards.core :as dc])
  (:require-macros
   [roamana.util :refer [s! rev]]
   [cljs.test  :refer [testing is]]
   [com.rpl.specter.macros  :refer [select select-one
                                    setval transform]]
   [reagent.ratom :refer [reaction]]
   [devcards.core :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))


(def tatom (atom {}))


(defn focus-append [this]
  (doto (.getDOMNode this)
    (.focus)
    (.setSelectionRange 100000 100000)))



(defn focus-append-input [m]
  (r/create-class
   {:display-name "focus-append-component"
    :component-did-mount focus-append
    :reagent-render
    (fn focus-append-input-render [m]
      [:input
       (merge
        {:type "text"
         :name "text"
         :style {:width "100%"}}
        m)])}))



(register-sub
 ::conn
 (fn [db]
   (reaction (:ds @db))))


#_(defn add-child [db]
   (let [conn (:ds db)
         current (subscribe [::active-entity])
         kid  (select-one [:tempids (sp/keypath -1)] 
                          (d/transact! conn [{:db/id -1
                                              :node/type :text
                                              :node/text "New Node"}
                                             [:db/add @current :node/children -1]]))]
     (do
       (dispatch [::state-from-conn]) 
       (when (= 3 (:depth db))
         (dispatch [::set-root @current]))
       (dispatch [::inc-depth]))
     db))



(defn add-child [conn id]
  (d/transact! conn [{:db/id -1
                      :node/type :text
                      :node/text "New Node"}
                     [:db/add id :node/children -1]]))


(defn n1 [n pid lstate]
  (let [conn (subscribe [::conn])]
    (fn [n pid lstate]
      (let [id (:db/id n)
            text (:node/text n)]
        [:div.tree
         [:div.flex.node-header
          [:button.circle]
          (if (= id (:focused @lstate))
            [focus-append-input
             {:value text
              :on-key-down (fn [e]
                             (do
                               
                               (case (->> e
                                          .-which
                                          u/key-name)
                                 "ENTER" (s! lstate (setval :focused nil))
                                 "TAB" (js/alert (add-child @conn id))
                                 "ESC" (js/alert text)
                                 :else)))
              :on-change
              #(dispatch [::q/transact [{:db/id (:db/id n)
                                         :node/text (.. % -target -value)}]])}]
            [:div
             (u/c #(s! lstate
                       (setval :focused id)))
             text])]
         [:div.leaf
          [:div.text
           (or
            (:node/body n)
            )
           (if-let [parents  (:node/_children n)]
             (for [p parents
                   :let [p' (:db/id p)]
                   :when (not (= p' pid))]
               [:button p']))]
          (if-let [children (:node/children n) ] 
            (for [c children]
              ^{:key (:db/id c)}[n1 c id lstate]))]
         ]))))


(defn n0 [lstate]
  (let [conn @(subscribe [::conn])
        n (posh/pull conn '[:node/text 
                            :node/body
                            :node/_children {:node/children ...}] 2)]
  (fn [lstate]
    [:div 
     [n1 @n 0 lstate]])))


(defcard-rg n0
  [n0 tatom]
  tatom
  {:inspect-data true
   :history true})




(defn outline1 [tatom]
  (let [d (subscribe [::q/active-entity])
        dv (subscribe [::q/pull] [(reaction 1)])]
    (fn [tatom]
      [:div

       [:button
        (u/c #(s! tatom
                  (setval :active @dv)))
       "c"]
       
      [:button
        {:on-click #(s! tatom 
                        (setval :a "b")
                        (setval :b  "c"))}
       "b"] 
       [:button
        {:on-click #(swap! tatom 
                           (fn [m]
                             (setval :a "a" m)))}]
      (pr-str @tatom)
      [q/node d]
       ]
      )))






(defcard-rg ahh
  [outline1 tatom]
  tatom
{:inspect-data true
 :history true})











(register-sub 
 ::text-nodes
 (fn [db]
   (let [conn (:ds @db)]
     (posh/q conn '[:find (pull ?e [:db/id {:node/children ...}])
                    :where [?e :node/text ?text]]))))

(register-sub
 ::nodes
 (fn [db [_ id]]
   (let [conn (:ds @db)]
     (posh/pull conn '[:db/id {:node/children ...}] id))))

(register-sub
 ::search
 (fn [db]
   (reaction (:roamana.search/search @db "node"))))


(register-sub 
 ::search-results
 (fn [db _ [search]]
   (let [conn (:ds @db)]
     (posh/q conn '[:find (pull ?e [:db/id :node/text {:node/children ...}])
                    :in $ ?strfn ?string
                    :where 
                    [?e :node/text ?text]
                    [(?strfn ?text ?string)]
                    ]
             (fn [a b] (str/includes? a b))   
             search))))

#_(deftest import
  (testing "imports"
      (let
          [d (subscribe [:roamana.search/active-entity])
           dv (subscribe [:roamana.search/apull][d])
           search (subscribe [::search])
           kids (subscribe [::nodes 0])
           search-nodes (subscribe [::search-results][search])
           nodes (subscribe [::text-nodes])]
        (is (s/valid? integer? @d))
        (is (= " " @kids))
        (is (= " " @nodes))
        (is (s/valid? string? @search))
        (is (= "" @search @search-nodes))
        (testing "string stuff"
          (is (subs "abcd" ""))
          (is (str/includes? "abcd" "")))
        )
      ))



(defn search []
  (let [s (subscribe [::search])
        d (subscribe [:roamana.search/depth])]
    (fn []
      [:div.search 
       [:input#search
        {:value @s
         :on-key-press #(if (= (.-charCode %) 13)
                         (do
                           
                           #_(js/alert [@d @s])
                           (if (= -1 @d)
                             (dispatch [:roamana.search/transact [{:db/id -1
                                                     :node/text @s}]]))
                           (search/move-focus "note" %)))
         :on-change  #(do
                        (dispatch [:roamana.search/assoc :roamana.search/depth -1])
                        (dispatch [:roamana.search/assoc :roamana.search/search (->
                                                       %
                                                       .-target
                                                       .-value)]))}]
       [:button :a]
       [:button :b]])))


(defn simplegrid []
  (let [sstring (subscribe [::search])
        s (subscribe [::search-results] [sstring])]
    (fn []
     
      [:div.grid-frame
        [search]
       [:div.outline
        {:style {:display "grid" 
                 :grid-template-rows (str "repeat("
                                          (count @s)
                                          ", [row] 0.2fr)")}}
        (for
            [[pos n] (map-indexed vector @s)]
            ^{:key (:db/id n)} [:div (pr-str n)])
        ]])))


#_(defcard-rg sea
  [simplegrid])


(register-sub
 ::conn
 (fn [db]
   (reaction (:ds @db))))

(defn singleoutline []
  (let [conn (subscribe [::conn])
        node (posh/pull @conn '[:db/id :node/text {:node/children ...}] 0)]
    (fn []
      [:div (pr-str @node)])))


#_(defcard-rg outline
  [singleoutline])


(declare node-simple)

(defn node [idratom]
  (let [n (subscribe [:dynamic/pull-all '[:db/id 
                                          :node/text 
                                          :node/body
                                          {:node/children ...}]]
                     [idratom])]
    (fn []
       [node-simple @n])))




(defn node-simple-edit [n]
  (let [lstate (subscribe [::key :lstate])]
    (fn [n]
      [:div {:style  {
                      :display "flex"
                      :background-color "grey"
                      :height  "100%"
                                        ;  :width "100%"
                                        ; :overflow "scroll"
                                        ; :padding "5px"
                      :flex-flow "column"}}
       
       [:div.flex.node-header
        [:button.circle
         {:on-click #(do
                       (dispatch [::setval [:lstate :focused] nil])
                       (dispatch [::setval [:lstate :adding-child] nil])
                       
                       #_(dispatch [::setval [:lstate :locked] true]))
          }
         ]
        (:node/text n)]
       
       [:div {:style {
                                        ;      :display "flex"
                      :margin-left "20px"
                      :border-left "1px solid #9b9b9b"
                                        ;     :background-color "blue"
                      :flex-flow "column"
                      
                      }}
        [:div.text
         (:node/body n)]
        
        (if-let [child-edit-id (:adding-child @lstate)]
          (if (= child-edit-id (:db/id n))
            [:textarea
             ])
          [:button {:on-click #(do
                               (dispatch [::setval [:lstate :adding-child] (:db/id n)]))
                  }
         "add Child"]
          )
        (for [c (:node/children n)]
          ^{:key (:db/id c)}[node-simple-edit c])]


       ])))




(deftest my
  (testing  "lstate"
    (is (not (= 1 @(subscribe [::q/key :lstate]))))
      )


)


(def v {:inspect-data true
        :history true})



(def schema {:node/children {:db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/many}
             :node/child-order  {:db/cardinality :db.cardinality/one}})


(defonce lconn (d/create-conn schema))
(posh! lconn)
(def cc (cursify lconn))


(def  sample
  [{:db/id 1
    :node/text "First Node"
    :node/children #{2 3 4 5 6 7}
    :node/order [7 6 5 4 3 2]}
   {:db/id 2
    :node/text "Second"
    :node/children  #{3 4 5 6 7}
    :node/order [3 5 7 4 6]}
   {:db/id 3
    :node/text "3"}
   {:db/id 4
    :node/text "4nod"}
{:db/id 5
    :node/text "4nod1"}
{:db/id 6
    :node/text "Second"}
{:db/id 7
    :node/text "Second"}]
)


(d/transact! lconn sample)


(defn c-path [id]
  [:node/children
   ALL
   (fn [c]
     (= (:db/id c) id))])


(declare ordered)

(defn o1 [n & [order]]
  (fn [n & [order]]
      (let [ordered-kids (map    
                          (fn [eid]
                            (filter #(= eid
                                        (:db/id %)))
                            (:node/children n))
                          (:node/order n))]

        ^{:key (:db/id n)} 
        [:div.tree
         [:h1 (:db/id n)]
         (if order 
           [:div [:button 
                  (u/c #(js/alert order))
                  :inc]
            [:button 
             (u/c #(js/alert order))
             :dec]]
           )
         (pr-str n)
            [:div.leaf
             (for [kid (:node/order n)
                   :let [k
                         (select-one
                          [(c-path kid)] n)]]
               [o1 k (:node/order n)])]
            ])))



(defn ordered []
  (let [n (posh/pull lconn '[:node/text
                             :node/order
                             {:node/children ...}] 1)]
    (fn []
      [o1 @n])))



(defcard-rg orderr
  "a"
  [ordered]
  {:inspect-data true
   :history true})




(defn ainc [arr x] 
  (let [v (clj->js arr)
        xpos (.indexOf v x)]
    (if-let [swapval (aget v (dec xpos))]
      (do (aset v xpos swapval)
          (aset v (dec xpos) x)
          (js->clj v))
        arr)))

(defn adec [arr x] 
  (let [v (clj->js arr)
        xpos (.indexOf v x)]
    (if-let [swapval (aget v (inc xpos))]
      (do (aset v xpos swapval)
          (aset v (inc xpos) x)
          (js->clj v))
      arr)))





(deftest moving-vec
  (testing "transform"
    (is (= [1 2 3 4 5]  
           (select-one 
            (sp/srange-dynamic #(.indexOf % 1) #(.indexOf % 6)) (range 15))))
    (is (= [1 2 3]  
           (select-one 
            (sp/srange-dynamic (fn [_] 0) #(.indexOf % 4)) (range 1 15)))
        )
    (is (= [4 5 6]  
           (select-one 
            (sp/srange-dynamic #(.indexOf % 4) #(count %)) (range 1 7)))
        )   
    (is (= [0 1 2]
           (adec [0 2 1] 2)))
   (is (= [0 1 2]
           (ainc [1 0 2] 0)))
    (is (= [0 1 2]
           (ainc [0 1 2] 0)))
    (is (= [0 1 2]
           (adec [0 1 2] 2))))
  )



(defn o2 [id]
  (let  [n (posh/pull lconn '[:node/text
                             :node/order
                              {:node/children 1}] id)]
    (fn []
      (let [n @n
            ordered-kids (map    
                          (fn [eid]
                            (filter #(= eid
                                        (:db/id %)))
                            (:node/children n))
                          (:node/order n))
            id (:db/id n)
            order (:node/order n)]

        ^{:key id} 
        [:div.tree
         [:strong id]
         [:strong (:node/text n)]
         (if order
           (pr-str order))
         [:div.leaf
          (for [kid order
                :let [k
                      (select-one
                       [(c-path kid)] n)
                      ]]
            ^{:key (str kid 'b)} [:div
                 
                 [:div {:style
                        {:border "1px solid green"
                         :display "flex"
                         :align-items "center"}}
                  [:button 
                        (u/c #(do
                               
                                (d/transact! lconn [{:db/id id
                                                     :node/order (ainc order kid)}])
                                )):inc]
                  [:button 
                   (u/c #(do
                                
                                (d/transact! lconn [{:db/id id
                                                     :node/order (adec order kid)}])
                                )):dec]
                  
                  [o2 kid]]
                 ])]
         ]))))



(defn order2 []
  (let [n (posh/pull lconn '[:node/text
                             :node/order
                             {:node/children ...}] 1)]
    (fn []
      [o2 1])))


(defcard-rg o
  [order2])
