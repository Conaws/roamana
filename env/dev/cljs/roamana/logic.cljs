(ns roamana.logic
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
            [roamana.zz :refer [cursify]]
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



(s/def :db/id integer?)


(s/def ::argument-type  #{::deductive ::inductive})

;  you could have a lock, like, if this is true, no way this can be false
;  
;

(s/def :arg/locks #{:if-true :only-if-true
                    :if-false :only-if-false})


(s/def :arg/impact (set (range -5 5)))

(s/def ::argument 
  (s/keys
                   :req [:db/id
                         :arg/premises 
                         :arg/conclusion]
                   :opt [:arg/lock
                         :arg/impact
                         :arg/impact-certainty
                         :arg/lock-certainty
                         :arg/impact-basis
                         :arg/lock-basis]))



(s/def ::conclusion (s/or :id  :db/id 
                          :prop ::propositions))

(s/def :prop/basis (s/coll-of ::argument []))

(s/def :prop/text string?)

(s/def :prop/certainty (s/and integer?))

(s/def ::propositions (s/* ::proposition))


(s/def ::proposition (s/keys
                      :req [:db/id]
                      :opt [:prop/text
                            :prop/basis
                            :prop/certainty]))


(def propositions
  [{:db/id 1
    :prop/text "Donald Trump Should Never Become President"}
   {:db/id 2
    :prop/text "Donald Trump is a racist"}
   {:db/id 3
    :prop/text "Racists cannot become President"}])

(def potential-premis-layouts [
                                {2 {:neccessary-for {:true-conclusion :true
                                                     :maybe-conclusion :not-false}
                                    :adds-weight  {:true-conclusion :true
                                                   :false-conclusion :false}}
                                 3 {}}])


(s/def ::impact-if-all-true #{:true/more
                              :true/less
                              :true/total
                              :true/none})

(def truth-map {0 :definitely-false
                1 :probably-false
                2 :likely-false
                3 :unknown
                4 :likely-true
                5 :probably-true
                6 :definitely-true})

(def truth-map 
  {0 :definitely-not
                1 :probably-not
                2 :likely-not
                3 :unknown
                4 :likely
                5 :probably
                6 :definitely})





(deftest a
  (testing "props"
    (is (s/valid? ::propositions propositions))
    (is (s/valid? ::proposition
               {:db/id 1
                :prop/text "Donald Trump Should Never Become President"}))
    (is (s/valid? ::argument
               {:db/id 4
                :arg/premises #{2 3}
                :arg/conclusion 1
                :arg/impact 3 
                :arg/impact-certainty 3
                }))))


(def schema {:prop/basis {:db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/many}
             :arg/premises  {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}
             :arg/conclusion {:db/valueType :db.type/ref
                              :db/cardinality :db.cardinality/one}})


(def lconn (d/create-conn schema))
(posh! lconn)
(def cc (cursify lconn))



(defcard-rg ent
  [:div 
   (pr-str @lconn)]
  cc
  {:inspect-data true
   :history true
})
