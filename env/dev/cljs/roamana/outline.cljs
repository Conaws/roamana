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
            [cljs.spec  :as s]
            [clojure.string :as str]
            [cljs.spec.impl.gen :as gen]
            [devcards.core :as dc])
  (:require-macros
   [cljs.test  :refer [testing is]]
   [com.rpl.specter.macros  :refer [select select-one
                                    setval transform]]
   [reagent.ratom :refer [reaction]]
   [devcards.core :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))



(defmacro mk [arg] `(dc/mkdn-pprint-source ~arg))


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


(defcard-rg sea
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


(defcard-rg outline
  [singleoutline])
