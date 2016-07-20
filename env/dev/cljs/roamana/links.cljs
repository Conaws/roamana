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


(def valid-link-regex0 #"\[\[((\w+(?:\s\w+)?)+)\]\]")

(def valid-link-regex #"\[\[([^\[]+)\]\]")

(def valid-link-regex2 #"\[\[([\w\s]+)\]\]")

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(s/def ::email-type (s/and string? #(re-matches email-regex %)))


(s/def ::regex #(= (type %) js/RegExp))

(s/def ::link-type (s/and string?  #(re-matches valid-link-regex %)))

(str/split teststring valid-link-regex2)



#_(->>  (insta/parse  (insta/parser "
               text =  (link / link-vals)+
               link = '[[' link-vals ']]' 
               <w>  =  #'[\\s,]+'
               link-vals  = (word | w)+
               <word> =  #'[a-zA-Z]+'") teststring)

      (insta/transform {:link-vals str}))



(deftest specing
  (testing "linkparser"
    (is (s/valid? ::regex valid-link-regex))
    (is (s/valid? ::link-type "[[this is a note]]"))
    (is (not (s/valid? ::link-type "[[this is an incomplete ]]")))
    (is (not (s/valid? ::link-type "[[this is definintely not a note")))
    (is (= "[[link to another note]]" (ffirst (re-seq valid-link-regex teststring))))
    (is  (= "aa" (re-matches #"aa" "aa")))
    (is (s/valid? string? teststring))))


(str/split "[] a b c []" "[]")
(str/ends-with? "[] a b c []" "[]")

(re-seq  #"\[\]" "[] a b c []")

(s/def ::test #"(?:\[\[)(\s\S)+(?=\]\])")
( #"(?:\[\[)(\s\S)+(?=\]\])")


;;  this one works, but misses edge case of not ending in a space
(re-seq #"(?:\[\[)([^\[\]]+\S)\]\]" teststring) 



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

(embed-link valid-link-regex teststring)

(str/split teststring valid-link-regex)

(def split-on-links-regex #"\[\[[^\[]+\]\]")


(str/split teststring split-on-links-regex)


(condp type "a"
    js/String :a
    js/RegExp :b)


(re-seq valid-link-regex teststring)



(.toUpperCase  "a")



(vector? [])

(s/fdef link-generator 
        :args  (s/cat :link-title string?)
        :ret vector?)


(defn link-generator [link-title]
  [:span {:style 
          {:bcolor "blue"}}
   link-title])

#_(defcard-rg search
  [:div
   [search/search]])

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
   (embed-link2 valid-link-regex teststring)])
