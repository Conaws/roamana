(ns roamana.parser
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      if-path END cond-path
                                      must pred keypath
                                      collect-one comp-paths] :as sp]
            [cljs.spec  :as s]
            [cljs.pprint :refer [pprint]]
            [cljsjs.firebase]
            [cljs.spec.impl.gen :as gen]
            [roamana.util :as u :refer [c]
             :refer-macros [s!]]
            [clojure.set :as set]
            [clojure.string :as str]
            [roamana.core :as core])
  (:require-macros
   [cljs.test  :refer [testing is]]
   [com.rpl.specter.macros  :refer [select
                                    select-one
                                    setval defnav 
                                    defpathedfn
                                    defnavconstructor
                                    fixed-pathed-nav
                                    variable-pathed-nav
                                    transform declarepath providepath]]
   [reagent.ratom :refer [reaction]]
   [devcards.core
    :refer [defcard defcard-doc defcard-rg deftest]]))


(enable-console-print!)
(re-frame.utils/set-loggers! {:warn #(js/console.log "")})

(register-sub
 ::text
 (fn [db]
   (reaction (::text @db ""))))

(register-handler
 ::change-text
 (fn [db [_ text]]
   (assoc db ::text text)))



(defn add-tab [text start end]
  (str (subs text 0 start) 
                 "\t"  
                 (subs text end)))

(register-handler
 ::add-tab
 (fn [db [_ e end]]
   (let [text (::text db)]
     (assoc db ::text 
            (str (subs text 0 e) 
                 "\t"  
                 (subs text end))))))



(def sstring "nodeA\n\tnode1\n\t\tnodeB\n\n\n\t\tnodeC")



(defcard-rg sstringcard
  [:textarea sstring])


(register-handler
 ::fix-tree 
 (fn [db]
   (let [tree @(subscribe [::parsed-text])]
     (assoc db ::tree tree))))







(register-sub
  ::parsed-text
  (fn [db]
    (reaction (u/parsed (::text @db)))))


(defn tree-text []
  (let [text (subscribe [::text])
        p (subscribe [::parsed-text])]
    (fn []
      [:div
       [:div (pr-str @p)]
       [:textarea {:style {:width 500 :height 500}
                   :on-change #(do
                                 (dispatch [::change-text (-> %
                                                              .-target
                                                              .-value)])
                                 (dispatch [::fix-tree]))
                   :on-key-down #(case (.-which %)
                                   9 (do
                                       (dispatch [::add-tab
                                                  (-> % .-target .-selectionStart)
                                                  (-> % .-target .-selectionEnd)])
                                       (.preventDefault %))
                                   :else)
                   :value @text}]])))


(defcard-rg parser
  [tree-text])




(defn nodify [nseq]
  (loop [result []
         s nseq]
    (let[sa (first s)
         r (rest s)
         [children siblings] (split-with #(< (first sa) (first %)) r)
         answer     {:node (second sa)
                     :children-visible true}
         answer
         (if (< 0 (count children))
           (assoc answer :children (nodify children))
           (assoc answer :children children))]

      (if (< 0 (count siblings))
        (recur (conj result answer) siblings)
        (conj result answer)))))





(defn transform-depthvec [nodefn edgefn sibling-collector nseq]
  (loop [result []
         s nseq]
    (let[[pdepth ptitle] (first s)
         [children siblings] (split-with #(< pdepth (first %)) (rest s))
         answer   (nodefn ptitle)
         answer
         (if (seq children)
           (edgefn answer (transform-depthvec nodefn edgefn sibling-collector children))
           answer)]
      (if (seq siblings)
        (recur (sibling-collector result answer) siblings)
        (sibling-collector result answer)))))


(def trigger #{"@person" "@role"})

(defn starts [e] 
  (str/starts-with? e "@"))



(s/def ::not-trigger (s/and string? #(not (trigger %))))


(s/def ::edgeparse (s/cat
                    :type  ::trigger
                    :val   (s/* ::not-trigger)))


(s/def ::even-parse  (s/*
                      (s/or :edge  ::edgeparse
                            :node (s/spec (s/+ ::not-trigger)))))


(def link-regex #"(?:\[\[)([^\[\]]+\S)\]\]") 

(s/def ::link (s/and string?  #(re-matches link-regex %)))

(s/def ::string string?)

(defn link-gen []
  (->> (s/gen string?)
       (gen/fmap #(str % %))))

(pprint (nodify (u/parsed sstring)))

(pprint (starts "@abc"))
(pprint (s/exercise ::string 10 {::string link-gen}))


