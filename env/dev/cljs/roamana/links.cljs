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


(def teststring "this is a test]] of an [[nvalt]] [[:note]] [[link to another note]] with an [[incomplete link and a [[link that is mid edit ]]")


(def link-regex #"(?:\[\[)([^\[\]]+\S)\]\]") 

(s/def ::regex #(= (type %) js/RegExp))



(s/def ::link (s/and string?  #(re-matches link-regex %)))

(s/def ::unparsed-link-vec (s/cat :outermatch ::link
                                  :inner  string?))
(s/def ::unparsed-links (s/* (s/spec ::unparsed-link-vec)))



(deftest cccab
  (testing "indexOf"
    (let [links (re-seq link-regex teststring)]
      (is (s/valid? ::unparsed-links links))
      (is (= 4  (str/index-of "abcde" "e")))
      (is (= "abcd" (subs "abcdefg" 0 4))))))



(defn  linkparse [returnvec compared-link-vec]
  (let [target (last returnvec)
        [matcher returnval] compared-link-vec
        matchlength (count matcher)
        matchloc  (str/index-of target matcher)]
    (assert (s/valid? ::link matcher))
    (assert (s/valid? integer? matchlength))
    (conj (vec (butlast returnvec)) 
          (subs target 0 matchloc)
           "[[" 
           [:a {:on-click #(dispatch [:roamana.search/assoc 
                                      :roamana.search/search 
                                      returnval])
                :style {:cursor "pointer"}} 
            returnval] 
           "]]" 
          
           (subs target 
                (+ matchloc matchlength)))))


(defn embed0 [teststring] 
  (let [links (re-seq link-regex teststring)]
    (assert (s/valid? ::unparsed-links links))
    (reduce linkparse 
            [:div 
             {:content-editable "true"} teststring] links) ))



(defcard-rg firstembed
  [embed0 teststring])



(defn content-editable []
  (let   [d (subscribe [:roamana.search/active-entity])
          dv (subscribe [:roamana.search/apull][d])]
    (fn []
      [:div.grid-frame
       [search/search]
       [search/outline1]
       [:div.note
        [:div#note
         {:content-editable "true"
          :on-input #(do
                       #_(.preventDefault %)
                       (js/console.log (-> % .-target .-innerHTML))
                       (dispatch [:roamana.search/transact [{:db/id @d :node/body 
                                                             (-> % .-target .-innerHTML)
                                                             }]]))}
         (:node/body @dv "")
         ]]])))




#_(defcard-rg content-editable-disaster
  [content-editable])


(defn  linkparse2 [returnvec compared-link-vec]
  (let [target (last returnvec)
        [matcher returnval] compared-link-vec
        matchlength (count matcher)
        matchloc  (str/index-of target matcher)]
    (assert (s/valid? ::link matcher))
    (assert (s/valid? integer? matchlength))
    (conj (vec (butlast returnvec)) 
          (subs target 0 matchloc)
           "[[" 
           [:a {:on-click #(do
                             (search/move-focus "search")
                             (dispatch [:search/depth 0])
                             (dispatch [:roamana.search/assoc 
                                          :roamana.search/search 
                                          returnval]))
                :style {:cursor "pointer"}} 
            returnval] 
           "]]" 
          
           (subs target 
                (+ matchloc matchlength)))))


(defn embed [container parsestring]
  (let [links (re-seq link-regex parsestring)]
    (assert (s/valid? ::unparsed-links links))
    (reduce linkparse2 
            (conj container parsestring) links) ))



(defn note []
  (let   [d (subscribe [:roamana.search/active-entity])
          dv (subscribe [:roamana.search/apull][d])]
    (fn []
      (let [text (if (<= 0 @d) (:node/body @dv "") "whatuu")]
        [:div.grid-frame
         [search/search]
         [search/outline1]
         [:div.note  {:style
                      {:display "grid"
                       :justify-content "stretch"
                       :grid-template-columns "1fr 1fr"
                       :background-color "blue"
                       :grid-template-areas "'view view'
                                            'edit  edit'"}}
          (embed [:div {:style {:grid-area "view"
                                :background-color "white"}}]
                 text)
          (if (>= 0 @d)
            [:div#note]
            [:textarea#note
             {:value  text
              :style {:grid-area "edit"}
              :on-change #(do
                            (if (< 0 @d) (dispatch [:roamana.search/transact [{:db/id @d :node/body 
                                                                               (-> % .-target .-value)
                                                                               }]])))}
             ])]]))))




(defcard-rg  notesy
  [note])

(s/fdef linkparse :args (s/cat
                         :returnvector (s/and vector? #(-> (last %)
                                                           string?))
                           :links ::unparsed-links ))




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




(def ttransforms
  {"a\\S+" (fn [x] [:a (count x)])
   "b\\S+" (fn [x] [:b (clojure.string/upper-case x)])
   "c\\S+" (fn [x] [:c "hahaha"])})




;; do some whacky stuff...
(defn ttransform [s transforms]
  (let [ks (keys transforms)
;; put all the patterns into one big regex or of groups (aaa)|(bbbb)|(cccc)
        p (re-pattern (str "(" (clojure.string/join ")|(" ks) ")"))
;; split the string to get the unmatched parts
        [before & more] (clojure.string/split s p)]
    (into
      [:div before]
;; interleave the splits and replacements...
;; 1 more unmatched because splits  a,b,c  2 commas 3 letters
      (interleave
        (for [[match & groups] (re-seq p s)]
;; re-seq is riddiculous, it returns a sequence of the match+groups...
;; where groups contains the ()()() group matches...
;; in our case we are doing an or, so only one of the groups will be non-nil,
;; so we find the index of the non-nil match, that's the pattern we
;want to use to replace with
;; look up the replacement function
          ((transforms (nth ks (count (take-while nil? groups)))) match))
        more))))

(ttransform "aaa bbbb cccc defdgd " ttransforms)
