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
            [clojure.zip :as z]
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



(defonce data (atom (z/vector-zip [1 2 [3 4 [5 6]]])))

(defn render-h*** [v n]
  (fn [v n]
    [:div.full.bblack
     {:style {:display "flex"
              :background-color
              (if (= v n)
                "white"
                "grey")
              }}
     (if (vector? v)
       (for [[k e] (map-indexed vector v)]
         ^{:key (str e k v)}
         [:div.bblack
          {:style {:background-color "blue"
                   :margin "1%"
                   :overflow "scroll"
                   :flex 1 1 "20%"}}
          [render-h*** e n]])
       (pr-str v n))

     ]))


(defn i [f]
  (fn [x]
    (if (f x)
      (f x)
      x)))


(defn nextt [z]
  (if (z/end? (z/next z))
    z
    (z/next z)))

(def action-map
  {:next nextt
   :prev (i z/prev)
   :up (i z/up)
  :down (i z/down)
  :right (i z/right)
  :left (i z/left)
  :remove (i z/remove)
  :splice (fn [z]
    (let [n (z/node z)]
      (z/replace z [n])))
  :insert-c (fn [z]
    (z/insert-child z (count (flatten (z/root z)))))})


(defn z-buttons [zatom]
  (fn [zatom]
    [:div.flex {:style {:display "flex"
                        :width "200px"
                        :height "200px"
                        :justify-content "center"
                 ;       :align-items "center"
                        :flex-flow "row wrap"
                        }}
     [:button {:on-click #(swap! zatom nextt)}
      "next"]
     [:button {:on-click #(swap! zatom (i z/prev))}
      "prev"]
     [:button {:on-click #(swap! zatom (i z/up))}
      "up"]
     [:button {:on-click #(swap! zatom (i z/down))}
      "down"]
     [:button {:on-click #(swap! zatom (i z/right))}
      "right"]
     [:button {:on-click #(swap! zatom (i z/left))}
      "left"]
     [:button {:on-click #(swap! zatom (i z/remove))}
      "remove"]
     [:button {:on-click #(swap! zatom
                                 (fn [z]
                                   (let [n (z/node z)]
                                     (z/replace z [n]))
                                   ))}

      "replace"]
     [:button {:on-click (fn [e]
                           (swap! zatom
                                  (fn [z]
                                    (z/insert-child z (count (flatten (z/root z)))))))}
      "insert-child z"]
     [:button {:on-click (fn [e]
                           (swap! zatom
                                  (fn [z]
                                    (z/append-child z 1))))}
      "append-child 1"]]
    
    ))


(defcard-rg h***card
  [render-h*** [1 2 3 [1 2] 4]])

(defn zrender [zatom]
  (fn [zatom]
    [:div
     [:h1 (pr-str (z/node @zatom))]
     [z-buttons zatom]
     [render-h*** (z/root @zatom) (z/node @zatom)]]
    ))



(defcard-rg test
  [zrender data]
  data
  {:inspect-data true
   :history true})




(defn zipm [f x]
  (loop [z x]
    (if (identical? (z/next z) z)
      (z/root z)
      (if (z/branch? z)
        (recur (z/next z))
        (recur (-> z (z/edit f) z/next))))))



(defn vec-zip [data]
  (z/zipper vector?
            seq
            (fn [old new]
              (vec new))
            data))



(def ast-data
  {:op :if
   :children [:test :then :else]
   :test {:op :eq
          :children [:a :b]
          :a {:op :const
              :val 42}
          :b {:op :const
              :val 42}}
   :then {:op :const
          :val "true"}
   :else {:op :const
          :val "false"}})


(defn ast-zip [data]
  (z/zipper :children
            (fn [node]
              (map node (:children node)))
            (fn [old new]
              (merge old
                     (zipmap (:children old)
                             new)))
            data)
  )


(defonce astatom (atom (ast-zip ast-data)))

(defn ast-part [ast x]
  [:div.bblack
   {:style {:margin-left "15px"
            }}
   (if (= ast x)
     "me")
   [:h3 (pr-str (:op ast))]
   (when-let [v (:val ast)]
     [:h4 v])
   (for [c (map ast (:children ast))]
     [ast-part c x])])

(defn ast-render [zatom]
  (fn [zatom]
    [:div (pr-str @zatom)
     [z-buttons zatom]
     [ast-part (z/root @zatom)
      (z/node @zatom)]
     ]))


(defcard-rg astcard
  [ast-render astatom]
  astatom
  {:inspect-data true
   :history true})






(declare render-frame** buttons t4 clean-top)

(def frame (atom {:active-frame 1
                  :frames [1 2 3]}))

(defn dec-button [frame]
  (fn [frame]
    (let [numbers (select [ATOM :frames (walker int?)] frame)
          next-dec (apply min (filter #(> (:active-frame @frame) %) numbers))]
      [:button {:on-click
                #(do (pprint next-dec)
                     (if (not (nil? next-dec))
                       (swap! frame assoc :active-frame next-dec)))}
       :new-dec])))



(defn inc-button [frame]
  (fn [frame]
    (let [numbers (select [ATOM :frames (walker int?)] frame)
          top-number (apply max numbers)
          next-inc (apply min (filter #(< (:active-frame @frame) %) numbers))]
      [:button {:on-click
                #(do (pprint next-inc)
                     (if (not (nil? next-inc))
                       (swap! frame assoc :active-frame next-inc)))}
       :new-inc])))



(defn render-h** [v]
  (fn [v]
    [:div.full
     {:style {:display "flex"
              }}
     (for [e v]
       ^{:key (str e v)}
       [:div.bblack
        {:style {:background-color "blue"
                 :overflow "scroll"
                 :flex 1 1 "20%"}}
        [render-frame** e]])]))

(defn render-v** [v]
  (fn [v]
    [:div.full.white
     {:style {:display "flex"
              :flex-direction "column"
              }}
     (for [e v]
       ^{:key (str e v)}
       [:div.bblack
        {:style {:overflow "scroll"
                 :flex 1 1 "5px"}}
        [render-frame** e]])]))

(defn render-frame** [e]
  (fn [e]
    (cond
      (list? e)
      [render-h** e]
      (vector? e)
      [render-v** e]
      :else
      [:div.full
       (if (= e (:active-frame @frame))
         [:h1 "here"])
       e
       ])))


(defn delete-button [framestate]
  [:button {:on-click
            #(swap! framestate
                    (fn [s]
                      (->> s
                       (setval :frames
                               (->> (:frames s)
                                    (t4 (:active-frame s))
                                    (clean-top)))
                       (transform :active-frame inc))))
            }
   "DELETE"]
  )


(defn mainframe** [frame]
  (fn [frame]
    [:div
     (pr-str @frame)
     [delete-button frame]
     [inc-button frame]
     [dec-button frame]
     [buttons frame]
     [:div.mainframe
      {:style {:display "flex"}}
      (for [e (:frames @frame)]
        ^{:key (str e)}
        [:div.bblack
         {:style {:background-color "blue"
                  :overflow "scroll"
                  :flex 1 1 "20%"}}
         [render-frame** e]])]]))


(defcard-rg newtt
 [mainframe** frame] 
  frame
  {:inspect-data true
   :history true}
  )





(defn clean-top  [v]
  (transform ALL
             (fn [x]
               (if (coll? x)
                 (if  (= 1 (count x))
                   (first x)
                   (clean-top x))
                 x))
             v))






(defn remove-y [x]
  (fn [v]
    (cond->> v
      :true (filter #(not= x %))
      (vector? v) vec)))


(declarepath AllVectors)
(providepath AllVectors
             (if-path coll?
                      (stay-then-continue
                       ALL
                       AllVectors)))

(defn t4 [a v]
  (transform AllVectors (remove-y a) v))


(defpathedfn remove-x-path [x]
  (filterer #(not= x %)))

(defn t3 [v a]
  (select [
           (remove-x-path a)
           ALL
           (if-path vector?
                    (remove-x-path a)
                    STAY)] v))




(defn t2 [v a]
  (select [
               (sp/filterer #(not= a %))
               ALL (if-path vector?
                        (sp/filterer
                         #(not= a %))
                        STAY)] v))


(deftest test2
  (testing "removal"
    (is
     (= [1 2 '(3) [4 [6]]]
        (t4 5 [1 2 (list 3 5) [4 [5 6] 5] 5])))
    (is
     (= [1 2 3 [4]]
        (t2 [1 2 3 [4 5]] 5)))
    (is
     (= [1 2 3 [4]]
        (t3 [1 2 3 [4 5]] 5)))))


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




(def lorem (apply str (repeat 200 "lorem hey impsum ")))

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



