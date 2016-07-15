(ns roamana.popover
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
            [datascript.transit :as dt]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      if-path END cond-path
                                      must pred keypath
                                      collect-one comp-paths] :as sp]
            [roamana.logger :refer [all-ents]]
            [roamana.views :refer [main-view logmap todo-create] :as views]
            [reagent.session :as session]
            [keybind.core :as key]
            [cljs.spec  :as s]
            [cljsjs.rangy-textrange]
            [cljsjs.rangy-core]
            [roamana.zz :refer [cursify]]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select setval defnav 
                                    select-one
                                    defpathedfn
                                    defnavconstructor
                                    fixed-pathed-nav
                                    variable-pathed-nav
                                    transform declarepath providepath]]
   [reagent.ratom :refer [reaction]]
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))


(enable-console-print!)

(def schema {:node/children {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}})




(def lorem 

"lorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnoj
lorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem [[embed]] ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnoj"
)






(defcard-rg aaa
 "aaa")


(defn  handlemouseup [e lstate]
  (let [selection (js/document.getSelection)]
    (if (< 0 (-> selection .toString .-length))
      (let [selectionBox (->> (.getRangeAt selection 0)
                              .getBoundingClientRect)
            targetBox (-> e 
                          .-target
                          .getBoundingClientRect)
            newTop  (-  (.-top selectionBox)
                        (.-top targetBox))]
        (js/alert selection)
        (js/alert newTop)
        (swap! lstate assoc-in 
               [:popBox :top] newTop)
        (swap! lstate assoc-in 
               [:popBox :left] (- (.-left selectionBox)
                                  (.-left targetBox)))
        ))))






(defn popover []
  (let [lstate (atom {:show-pop true
                      :popBox  {:top 0
                                :left 0}})]
    (fn []
      [:div
       {:on-mouse-up #(do 
                          (handlemouseup % lstate))}
       lorem
       [:div.popover
        {:style {:visibility (if (:show-pop @lstate)
                            "visible"
                            "hidden")
                 :display  (if (:show-pop @lstate)
                            "inline-block"
                            "none")
                 :background-color "blue"
                 :position "absolute"
                 :top (get-in @lstate [:popBox :top])
                 :left (select-one [:popBox :left] @lstate)}}
        [:button "hey"]]])))


(defcard-rg pop
  [popover])






(defn  handlemouseup2 [e lstate]
  (let [selection (js/document.getSelection)]
    (if (< 0 (-> selection .toString .-length))
      (let [selectionBox (-> (.getRangeAt selection 0)
                             .getBoundingClientRect)
            selectionBoxes (-> (.getRangeAt selection 0)
                              .getClientRects)
            
            sellength (.-length selectionBoxes)
            firstsel (aget selectionBoxes 0)
            lastsel (aget selectionBoxes (dec sellength))
            targetBox (-> e 
                          .-target
                          .getBoundingClientRect)
            newTop  (-  (.-top selectionBox)
                        (.-top targetBox))]
        (js/console.log "target" targetBox)
        (js/console.log "first" firstsel)
        (js/console.log "last"  lastsel)
        (swap! lstate assoc-in 
               [:popBox :top] newTop)
        (swap! lstate assoc-in 
               [:popBox :left] (- (.-left firstsel)
                                  (.-left targetBox)))
        ))))






(defn popover2 []
  (let [lstate (atom {:show-pop true
                      :popBox  {:top 0
                                :left 0}})]
    (fn []
      [:div
       {:on-mouse-up #(do 
                          (handlemouseup2 % lstate))}
       lorem
       [:div.popover
        {:style {:visibility (if (:show-pop @lstate)
                            "visible"
                            "hidden")
                 :display  (if (:show-pop @lstate)
                            "inline-block"
                            "none")
                 :background-color "blue"
                 :position "absolute"
                 :top (get-in @lstate [:popBox :top])
                 :left (select-one [:popBox :left] @lstate)}}
        [:button "hey"]]])))





(defcard-rg pop2
  [popover2])



(defn  handlemouseup3 [e lstate]
  (let [selection (js/document.getSelection)]
    (if (< 0 (-> selection .toString .-length))
      (let [selectionBox (-> (.getRangeAt selection 0)
                             .getBoundingClientRect)
            selectionBoxes (-> (.getRangeAt selection 0)
                              .getClientRects)
            
            sellength (.-length selectionBoxes)
            firstsel (aget selectionBoxes 0)
            lastsel (aget selectionBoxes (dec sellength))
            targetBox (-> e 
                          .-target
                          .getBoundingClientRect)
            newTop  (-  (.-bottom selectionBox)
                        (.-top targetBox)
                        20)]
        (js/console.log "target" targetBox)
        (js/console.log "first" firstsel)
        (js/console.log "last"  lastsel)
        (swap! lstate assoc :show-pop true)
        (swap! lstate assoc-in 
               [:popBox :top] newTop)
        (swap! lstate assoc-in 
               [:popBox :left] (+ (- (.-left lastsel)
                                     (.-left targetBox))
                                  (.-width lastsel)
                                  45))
        )
      (swap! lstate assoc :show-pop false))))






(defn popover3 []
  (let [lstate (atom {:show-pop true
                      :popBox  {:top 0
                                :left 0}})]
    (fn []
      [:div
       {:on-mouse-up #(do 
                          (handlemouseup3 % lstate))}
       lorem
       [:div.popover
        {:style {:visibility (if (:show-pop @lstate)
                            "visible"
                            "hidden")
                 :display  (if (:show-pop @lstate)
                            "inline-block"
                            "none")
                 :background-color "blue"
                 :position "absolute"
                 :top (get-in @lstate [:popBox :top])
                 :left (select-one [:popBox :left] @lstate)}}
        [:button "hey"]]])))





(defcard-rg pop3
  [popover3])


(defcard-rg alignment
[:div.wrapper-alignment
 [:div.alignA
  (apply str  :a (take 200 lorem))]
 [:div.alignB
  (apply str :b (take 200 lorem))]
 [:div.alignC
  (apply str (take 200 lorem))] 
[:div.alignD
  (apply str (interleave (take 50 lorem) (repeat " ") (repeat :D)))]
[:div.alignE
  (apply str (interleave (take 10 lorem) (repeat :E)))]
[:div.alignF
  (apply str (take 200 lorem))]])



(defn navbar []
   [:div {:style {:grid-area "header"
                  :display "flex"
                  :justify-content  "space-around"
                  :background-color "green"}}
    [:button.box 1]
    [:button 2]
    [:button 3]])


(defcard-rg flex
  [:div.flexgrid00
   [navbar]
   [:div {:style {:grid-area "content"
                  :padding "1em"}}
    [:div.box {:style {:float "left"
                       :margin-top "4em"
                       :margin-right "2em"
                       :margin-bottom "4em"
                       :shape-outside "circle()"}} 4]
    lorem]
   [:div {:style {:grid-area "sidebar"}}
    "abcededf"]
   [:div {:style {:grid-area "footer"
                  :background-color "#4B4B4B"}}
    "efgh"]])









(defn get-text []
  (if-let [a (js/window.getSelection)]
    (js/alert a)))



(key/bind! "ctrl-i" :get-text #(get-text))
