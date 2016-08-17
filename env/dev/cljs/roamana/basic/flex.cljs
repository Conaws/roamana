(ns roamana.basic.flex
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      walker stay-then-continue 
                                      filterer transform* select*
                                      if-path END cond-path
                                     ATOM must pred keypath
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


(defn remove-y [x]
  (fn [v]
     (filterv #(not= x %) v)))

(declarepath AllVectors)
(providepath AllVectors
             (if-path vector?
                      (stay-then-continue
                       ALL
                       AllVectors)))

(transform AllVectors (remove-y 5) [1 2 3 [4 [5 6] 5] 5])

(defnav ALL-ELEM-SEQ []
  (select* [this structure next-fn]
           (mapcat (fn [e] (next-fn [e])) structure)
           )
  (transform* [this structure next-fn]
              (mapcat (fn [e] (next-fn [e])) structure)    
              ))


(transform [ALL-ELEM-SEQ (sp/selected? sp/FIRST even?)]
           (fn [[e]] (repeat e e))
           [1 2 3 4 5])

(mapcat reverse [[1 2][][1 2]] )



(defn clean-top  [v]
  (transform ALL
             (fn [x]
               (if (coll? x)
                 (if  (= 1 (count x))
                   (first x)
                   (clean-top x))
                 x))
             v))

(clean-top [1 [3] [2 3]])


(defn t1 [v x]
  (filter #(not= x %) v))


(defpathedfn remove-x [x]
  (filterer #(not= x %)))


(defn t3 [v a]
  (select [
           (remove-x a)
           ALL
           (if-path vector?
                    (remove-x a)
                    STAY)] v))

(deftest test1
  (testing "removal"
    (is
     (= [1 2 3 [4]]
        (t3 [1 2 3 [4 5]] 5)))))


(defn t2 [v a]
  (select [
               (sp/filterer #(not= a %))
               ALL (if-path vector?
                        (sp/filterer
                         #(not= a %))
                        STAY)] v))


(t2 [1 2 3 [4 5]] 5)




(declare render-frame*
         buttons)

(def frame2
  (atom {:active-frame 1
         :frames (list 1 2 [3 4] [5 6])})
  )

(defn render-h* [v frame]
  (fn [v frame]
    [:div.full
     {:style {:display "flex"
              }}
     (for [e v]
       ^{:key (str e v)}
       [:div.bblack
        {:style {:background-color "blue"
                 :overflow "scroll"
                 :flex 1 1 "20%"}}
        [render-frame* e frame]])]))

(defn render-v* [v frame]
  (fn [v frame]
    [:div.full.white
     {:style {:display "flex"
              :flex-direction "column"
              }}
     (for [e v ]
       ^{:key (str e v)}
       [:div.bblack
        {:style {:overflow "scroll"
                 :flex 1 1 "5px"}}
        [render-frame* e frame]])]))

(defn render-frame* [e frame]
  (fn [e frame]
    (cond
      (list? e)
      [render-h* e frame]
      (vector? e)
      [render-v* e frame]
      :else
      [:div.full
       (if (= e (:active-frame @frame))
         [:h1 "here"])
       e
       ])))




(defn mainframe [frame]
  (fn [frame]
    [:div
     (pr-str @frame)
     [:h1 (:active-frame @frame)]
     [buttons frame] 
     [:div.mainframe
      [render-frame* (:frames @frame) frame]
      ]]
    ))



(defcard-rg wstate3
  [mainframe frame2]
  frame2
  {:inspect-data true
   :history true})







(declare render-frame
         split-h split-v)

(def frame1
  (atom {:active-frame 1
         :frames (list 1 2 [3 4] [5 6])})
  )

(defn inc-frame [f]
  (transform [ATOM :active-frame] inc f))

(defn dec-frame [f]
  (transform [ATOM :active-frame] dec f))


(defn buttons [frame]
  [:div
   [:button
    (c #(dec-frame frame)) 
    :dec]
   [:button
    (c #(inc-frame frame)) 
    :inc]
   [:button
    (c #(s!
         frame
         ((fn [v]
            (transform :frames (partial split-v
                                        (:active-frame v)
                                        )
                       v)))
         ))
    "split vert"]
   [:button
    (c #(s!
         frame
         ((fn [v]
            (transform :frames (partial split-h
                                        (:active-frame v)
                                        )
                       v)))
         ))
    "split horiz"]]
  )


(defn mainframe0 [frame]
  (fn [frame]
    [:div
     (pr-str @frame)
     [:h1 (:active-frame @frame)]
    [buttons frame] 
     [:div.mainframe
      [render-frame (:frames @frame)]
      ]]
    ))

(defcard-rg wstate
  [mainframe0 frame1]
  frame1
  {:inspect-data true
   :history true})



(defcard-rg movement
  [:div.mainframe
   [render-frame
    (list 1 2 [3 4] (str 5 "  " lorem))]])




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



