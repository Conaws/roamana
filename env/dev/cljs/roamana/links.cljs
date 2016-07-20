(ns roamana.links
  (:require [reagent.core :as r :refer [atom]]
;            [re-frame.db :refer [app-db]]
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


(def teststring "this is a test]] of an [[nvalt]] [[:note]][[link to another note]] with an [[incomplete link and a [[link that is mid edit ]]")


(def link-regex #"(?:\[\[)([^\[\]]+\S)\]\]") 

(s/def ::regex #(= (type %) js/RegExp))
(s/def ::link (s/and string?  #(re-matches link-regex %)))






(deftest specing
  (testing "linkparser"
    (is (s/valid? ::regex link-regex))
    (is (s/valid? ::link "[[this is a note]]"))
    (is (not (s/valid? ::link "[[this is an incomplete ]]")))
    (is (not (s/valid? ::link "[[this is definintely not a note")))
    (is  (= "aa" (re-matches #"aa" "aa")))))








(s/fdef embed-link 
        :args  (s/cat 
                :link-matcher (s/or :link-regex  ::regex
                                    :link-str  string?)
                :full-str string?))


(defn embed-link [link-tar s]
  (if (str/includes? s link-tar)
      (->> (str/split s (re-pattern link-tar))
           (interleave (repeat [(keyword (str "a#" link-tar)) link-tar]))
           rest
           (into [:div]))
      s))

(s/instrument #'embed-link)







(condp type "a"
    js/String :a
    js/RegExp :b)









(s/fdef link-generator 
        :args  (s/cat :link-title string?)
        :ret vector?)


(defn link-generator [link-title]
  [:span {:style 
          {:bcolor "blue"}}
   link-title])




(defcard-rg links
  [:div
   (map link-generator ["ajj" "bb"])])

(defn embed-link2 [link-matcher text]
  (let [link-regex (condp type link-matcher
                     js/String (re-pattern link-matcher)
                     js/RegExp link-matcher)
        endstrue (str/ends-with? text text)
        ]
    (-> (str/split text link-regex)
         (interleave (select [ALL FIRST] text)))))




(deftest aa
  (testing "stitching"
    (let [s "a b cd e f g"]
      (is (str/ends-with? "abc de" #"\w"))
      (is (str/ends-with? s "g"))
      (is (=  s (str/join " " (str/split s  #"\s") )))
     (is (not (= s
             (apply str
                    (interleave (re-seq #"\w+" s)
                                (re-seq #" " s)
                                 
                                ))))))))




(defcard-rg link2
  [:div
   (embed-link2 link-regex teststring)])
