(ns roamana.outline
  (:require [reagent.core :as r :refer [atom]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY FIRST
                                      MAP-VALS LAST
                                      ATOM
                                      stay-then-continue 
                                      srange-dynamic
                                      NIL->VECTOR NIL->SET
                                      view walker
                                      transformed
                                      if-path END cond-path
                                      srange must pred keypath
                                      collect-one comp-paths]
             :as sp]
         
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
                                    setval transform
                                    defnav
                                    defpathedfn]]
   [reagent.ratom :refer [reaction]]
   [devcards.core :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(enable-console-print!)


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
             :node/order  {:db/cardinality :db.cardinality/one}})


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
    :node/text "3"
    :node/children #{4 5 6}
    :node/order [6 5 4]}
   {:db/id 4
    :node/text "4nod"
    :node/children #{5 6}
    :node/order [6 5]}
   {:db/id 5
    :node/text "4nod1"
    :node/children #{7}
    :node/order [7]}
   {:db/id 6
    :node/children #{7}
    :node/order [7]
    :node/text "Second"}
   {:db/id 7
    :node/text "Second"}])



(def set-example [{:db/id 8
                   :set/type :set/union
                   :set/members #{1 3}}
                  {:db/id 9
                   :set/type :set/difference
                   :set/outer #{1}
                   :set/removed #{2}}])






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
               ^{:key (str k "a")} [o1 k (:node/order n)])]
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





(register-sub
 ::sub-active
 (fn [db]
   (reaction (::sub-active @db 1))))

(register-handler
 ::sub-active
 (fn [db [_ v]]
   (setval  ::sub-active v db)))




(defn o2 [id]
  (let  [active (subscribe [::sub-active])
         n (posh/pull lconn '[:node/text
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
         (if (= @active id)
           [:button :A]
           [:button
            (u/c #(dispatch [::sub-active id])) id])
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




(defn pos1 []
  (let [main (posh/pull lconn '[:node/order
                             {:node/children ...}] 1)
        active (subscribe [::sub-active])]
    (js/console.log @active)))


(key/bind! "ctrl-o" ::p1 #(pos1))


(defn id-path [id]
 (fn [c]
   (= (:db/id c) id)))






(declare ordered-children)

#_(def examplvec [1 
                 [11 12] 
                 [[111 112] [121 122]]
                 [[[1111 1112] []] [[] [1221 1222]]]])



#_(deftest transformedpath
  (testing "specter/transformed path"

    (is (s/valid? vector?
                  (select [ALL odd?] (range 10))))
    (is (s/valid? vector?
                  (select-one (transformed [ALL odd?] inc) (range 10))))
    (is (s/valid? vector?
                  (select-one (transformed [ALL odd?] 
                                           (fn [x] (* 2 x))) (range 10))))
    (is (s/valid? vector?
                  (select-one (transformed [ALL odd?] 
                                           (partial * 2)) (range 10))))
    (is (s/valid? vector?
                  (select-one (transformed [ALL odd?] 
                                           (partial * 2)) (range 10))))
    (is (s/valid? vector?
                  (transform [(transformed [ALL odd?] (partial * 2)) ALL] #(/ % 2) (range 10))))))



;; bunch of stuff moved to plan.org



(defpathedfn repeat-path [walk-path end-path i]
  (if (= 0 i)
    end-path
    [walk-path (repeat-path walk-path end-path (dec i))]))


(defn followpath [walkpath endpath depth tree]
  (vec  (for [i (range depth)] 
          (select (repeat-path walkpath endpath i) tree))))






(defn ordered-children [n]
  (vec (for [kid (:node/order n)
             :let [k
                   (select-one
                    [(c-path kid)] n)]]
         k)))



(defn step [m]
  (transform (sp/walker #(:db/id %))
             (fn [m]
               (if (:node/order m)
                 (ordered-children m)
                 []))
             m ))

(defn final-step [m]
  (transform (sp/walker #(:db/id %))
               (fn [m]
                 (if (:node/order m)
                   (transform [(sp/view ordered-children) ALL] :db/id m)
                   []))
               m))


(defn vectorify [n]
  (let [o  (:db/id  n)
        o1 (-> n final-step)
        o2 (-> n step final-step )
        o3 (-> n step step final-step)
        o4 (-> n step step step final-step)]
    [o1 o2 o3 o4]))

(declare zz4column)

(def dpath (atom [0]))


(defn zzCell [dvec c]
  (fn [dvec c]
    (if (s/valid? vector? c)
      (if (empty? c)
        [:div ""]
        [zz4column dvec c])
      (if (= dvec @dpath) 
        [:div.zzcell 
         [search/Viewable (pr-str dvec)]]
        [:div.zzcell 
         (pr-str dvec)]))))



(defn zz4column [cid cents]
  (fn [cid cents]
    (assert (vector? cid))
    [:div
     {:style
      {:display
       "flex"
       :flex-direction "column"
       :align-items "center"
       :justify-content "center"}
      
      }
     (for [[rid c] (map-indexed vector cents)
           :let [id (conj cid rid)]]
       ^{:key id} [zzCell id c]
       )]))


(def n4  (posh/pull lconn '[:node/order
                             {:node/children ...}] 1))

(register-sub
 ::n2
 (fn [_]
   (reaction (vectorify @n4))))

(defn zz4 []
  (let [n (posh/pull lconn '[:node/order
                             {:node/children ...}] 1)
        v (subscribe [::n2])]
    (fn []
      (let [v @v]
        [:div 
         [:div (pr-str v)]
         [:div
               {:style {:display "grid"
                        :background-color "blue"
                        :height "1000px"
                        :overflow "scroll"
                        :width "700px"
                        :grid-column-gap  "10px"
                        :grid-template-columns "repeat(4, [column] 1fr)"
                        }}
               (for [[col-id col] (map-indexed vector v)]
                 ^{:key col-id} [zz4column [] col])]]))))





(defcard-rg zz4card
  [zz4]
  dpath
  {:inspect-data true
   :history true})


(s/def ::array  js/Array.isArray )


(defn first-index
  ([f] (partial first-index f))
  ([f s]
   (count (take-while #(not (f %)) s)))
  ([f s i]
   (first-index f (drop i s))))

(defn last-index
  ([f] (partial last-index f))
  ([f s]
   (count (drop-while #(not (f %)) (reverse s))))
  ([f s i]
   (first-index f (drop i s))))




(deftest indexhelprs
  (testing "lastindex"
    (is (= 2 (last-index even? [1 1 2 2 2 1 1] 2)))))


(defn valid 
  ([a] (partial valid a))
  ([a b]
   (s/valid? a b)))

(s/def ::even (s/and integer? even?))
(s/def ::odd (s/and integer? odd?))
(s/def ::vec vector?)


(defn first-to-last [st ed]
  (sp/srange-dynamic
   (first-index #(valid st %))
   (last-index #(valid ed %))))

(defn kpdepth [d]
  (vec
   (for [i d]
     (keypath i))))

#_(defpathedfn r [walk-path end-path i]
  (if (= 0 i)
    end-path
    [walk-path (repeat-path walk-path end-path (dec i))]))

(let [a [1 2 3 [] 5 6 [] 7]
          b (clj->js a)
      c [[] [] [1 2 [3]]]]
  (select [(first-to-last ::odd ::vec)] a)
  (select-one [(keypath 2) (keypath 2)] c)
  (select-one [(kpdepth [2 2])] c)
)

(defn drop-lastv [v]
  (vec (drop-last v)))


(drop-lastv [0 1 2 3])

(defn dec-path [kpath]
  (if-let [l (select-one LAST kpath)]
    (if (>= 0 l)
      (drop-lastv kpath)
      (transform LAST dec kpath))))

(defn slideup [kpath dv]
  (loop [kpath kpath
         dvec dv]
    (assert (s/valid? (s/coll-of 
                       (s/or :v vector?
                             :i integer?)
                       :into []) kpath))
    (assert vector? dv)
    (if (seq kpath)
      (if-let [newpath (dec-path kpath)]
        (let [search-column (dec (count newpath))]
            (if-let [r (select-one [(keypath search-column) (kpdepth newpath)] dvec)]
              (if (integer? r)
                {:path newpath
                 :r r}
                (recur newpath dvec))
              ))
          "no")
      "no")))

(def dgrid1
  [[0 1 2 3] 
   [[] [] [20] [30]] 
   [[] [] [[200 201]] [[300 301]]] 
   [[] [] [[] [[2010]]]]])


;(map-indexed vector [0 1 2 3])












(deftest jsarrays 
  (testing "arrays"
    (let [a [1 2 3 [] 5 6 [1 2 3 [4]] 7]
          b (clj->js a)
          c (subscribe [::n2])
          d [[0 1 2 3] 
             [[] [] [20] [30]] 
             [[] [] [[200 201]] [[300 301]]] 
             [[] [] [[] [[2010]]]]]]
      (is (= 4 (.indexOf a 5)))
      (is (= 3 (.findIndex b array?)))
      (is (= 5 (.find (.slice b 3) integer?)))
      (is (= 5 (.find (-> b
                           js->clj
                           reverse
                           clj->js
                           (.slice 3)) integer?)))
      (is (vector? a))
      (is (s/valid? ::array b))
      (is (= 4 (select-one [(kpdepth [6 3 0])] a)))
      (is (= 4 (select-one [(kpdepth [0 3])] @c)))
      (is (= (select-one [(keypath 0) (kpdepth [2])] d)
           (:r (slideup [2 0] d))))
      (is (= (select-one [(keypath 1) (kpdepth [2 0])] d)
           (:r (slideup [2 1] d))))
      
      (slideup [1] d)
      )))


(defn fireslideup []
  (let [path dpath
        dvec (subscribe [::n2])
        ret (slideup @path @dvec)]
    (if ret
      (reset! path (:path ret)))))



(defn in-grid [path grid]
  (let [searchcol (dec (count path))]
      (if (select-one [(keypath searchcol)
                       (kpdepth path)] grid)
        path)))


(defn inc-path [depthpath grid]
  (let [newpath
           (transform LAST inc depthpath)]
    (or
     (in-grid newpath grid)
     depthpath)))


(defn slide [slidefn]
  (let [path dpath
        dvec (subscribe [::n2])]
    (transform ATOM
               (fn [s]
                 (or
                  (in-grid
                   (->> s
                        slidefn)
                   @dvec)
                  s))
               path)))


(defn dec-path2 [dpath]
  (if-let [l (select-one LAST dpath)]
    (if (>= 0 l)
      (drop-lastv dpath)
      (transform LAST dec dpath))))



(defn slideup2 []
  (slide dec-path2))

(defn slidedown []
  (slide (fn [dpath]
           (transform LAST inc dpath))))

(defn slideright []
  (slide (fn [dpath] (conj dpath 0))))

(defn slideleft []
  (slide (fn [e] (vec (drop-last e)))))








(key/bind! "up" ::up  #(slideup2))
(key/bind! "right" ::right  #(slideright))
(key/bind! "down" ::down  #(slidedown))
(key/bind! "left" ::left  #(slideleft))

