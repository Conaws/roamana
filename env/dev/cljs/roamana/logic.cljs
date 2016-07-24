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



(s/def ::id (s/or :tag string? 
                   :num integer?))


(s/def ::argument-type  #{::deductive ::inductive})

(s/def ::argument (s/keys
                   :req [::id
                         ::premises 
                         ::conclusion]))



(s/def ::conclusion (s/or :id  ::id 
                          :prop ::propositions))

(s/def ::basis (s/coll-of ::argument []))

(s/def ::text string?)

(s/def ::certainty (s/and integer?))

(s/def ::propositions (s/* ::proposition))


(s/def ::proposition (s/keys
                      :req [
                            ::id]
                      :opt [::text
                            ::basis
                            ::certainty]))


(def propositions
  [{::id 1
    ::text "Donald Trump Should Never Become President"}
   {::id 2
    ::text "Donald Trump is a racist"}
   {::id 3
    ::text "Racists cannot become President"}])





(s/explain ::proposition
         {::id 1
          ::text "Donald Trump Should Never Become President"})



(s/explain ::argument
          {::id 4
           ::premises {2 {:neccessary-for {:true-conclusion :true
                                           :maybe-conclusion :not-false}
                          :adds-weight  {:true-conclusion :true
                                         :false-conclusion :false}}
                       3 {}}
           ::conclusion 1
           ::certainty-given-true-premises 100
           })


(def schema {:node/children {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}})


(defonce lconn (d/create-conn schema))
(posh! lconn)
(def cc (cursify lconn))



(defcard-rg ents
  [:div "hey"]
  cc
  {:inspect-data true
   :history true
})
