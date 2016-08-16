(ns roamana.basic.flex
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
            [cljsjs.firebase]
            [roamana.util :refer [c]
             :refer-macros [s!]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint] ]
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
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))


(def lorem (apply str (repeat 200 "lorem hey impsum ")))

(defn split-on-x-with [f x v]
  (transform (sp/walker #(= x %))
             (fn [x]
               (f x (inc (count (flatten v)))))
             v)
)


(def split-h (partial split-on-x-with list))
(def split-v (partial split-on-x-with vector))

(deftest splitting
  (testing "split-transforms"
    (is
     (= [1 2 '(3 5) 4]
      (split-h 3 [1 2 3 4])))
    (is
     (= [1 '([3 5] 4) 2]
        (split-v 3 [1 '(3 4) 2])))))



(split-v 3[1 2 3 4])

(let [v [1 3 '(4 2) 5 [6 '(7 8)]]]
  (transform (sp/walker #(= 2 %))
             (fn [x]
               [x (inc (count (flatten v)))])
             v)
  )


(pprint lorem)

(declare  per-of render-frame)

(defn render-h [v]
  (fn []
    [:div.full.green
     {:style {:display "flex"
              }}
     (for [e v
           :let [h (per-of v)]]
       ^{:key (str e v)}
       [:div.bblack
        {:style {:overflow "scroll"
                 :flex 1 1 "20%"}}
        [render-frame e]])]))

(defn render-v [v]
  (fn []
    [:div.full.white
     {:style {:display "flex"
              :flex-direction "column"
              }}
     (for [e v
           :let [h (per-of v)]]
       ^{:key (str e v)}
       [:div.bblack
        {:style {:overflow "scroll"
                 :flex 1 1 "5px"}}
        [render-frame e]])]))

(defn render-frame [e]
  (fn []
    (cond
      (list? e)
      [render-h e]
      (vector? e)
      [render-v e]
      :else
      [:div.full (pr-str e)])))


(defcard-rg ter2
  [:div.green.flex
   {:style {:width "100%"
            :height "700px"
            }}
   [render-frame [1 2 lorem (list 1 lorem 3) 4]]]
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn per-of [x]
  (str (* 100 (/ 1 (count x))) "%"))

(declare render-frame0)

(defn render-h0 [l]
  (fn []
    [:div.full.flex.green
           (for [e l]
             ^{:key (str e l)}
             [:div.bred.full
              {:style {
                       :flex 1 1 (per-of l)}}
              [render-frame0 e]
              ]
             
             )]))


(defn render-v0 [v]
  (fn []
    [:div.full.white
     {:style {:display "flex"
              :flex-direction "column"
             }}
     (for [e v
           :let [h (per-of v)]]
       ^{:key (str e v)}
       [:div.bblack
        {:style {:flex 1 1 "20%"}}
       
        [render-frame0 e]])]))



(defn render-frame0 [e]
  (fn []
    (cond
      (list? e)
      [render-h e]
      (vector? e)
      [render-v e]
      :else
      [:div.full (pr-str e)])))


(defcard-rg ter
  [:div.green.flex
   {:style {:width "100%"
            :height "700px"
            }}
   [render-frame [1 2 3 4]]]
  )




(defcard-rg text
  [:div
   {:style {:width "100%"
            :height "700px"
            :background-color "blue"}}
   [render-h '(1 (1.5 1.7) 2 [3 4 5 6 7 8 (9 10 [11 12])])]]
   )



