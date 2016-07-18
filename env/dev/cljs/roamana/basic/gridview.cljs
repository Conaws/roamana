(ns roamana.basic.gridview
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
            [roamana.zz :refer [cursify]]
            [goog.i18n.DateTimeFormat :as dtf]
            [roamana.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select setval defnav 
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


(defonce conn (d/create-conn schema))
(posh! conn)
(def cc (cursify conn))


(def lorem 

"lorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnoj
lorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem [[embed]] ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnojlorem ipsum impsalklk lkajklag lkagjlketa lkjalkdonovith ooOHn goNggan oagnoj"
)

(defn embed []
  [:div.wrapper01
   [:div  lorem]
   [:div  lorem]
   ])

(defcard-rg embed-test
  [embed]
 )


(defcard-rg  test2
  [:div.wrapper02
   [:div.mainheader02
    "abaced"]
   [:div.content02
    lorem]
   [:div.panel02
    lorem]
   [:div.footer02
    "efg"]])





(defcard-rg  test3
  [:div.wrapper03
   [:div.mainheader03
    "abaced"]
   [:div.content03
    lorem]
   [:div.panel03
    lorem]
   [:div.footer03
    "efg"]])




(defcard-rg grid-areas00
  [:div.wrapper-areas00
   [:div {:style {:grid-area "header"
                  :background-color "green"}}
    "abcde"]
   [:div {:style {:grid-area "content"}}
    lorem]
   [:div {:style {:grid-area "sidebar"}}
    lorem]
   [:div {:style {:grid-area "footer"
                  :background-color "#4B4B4B"}}
    "efgh"]
])

(defcard-rg grid-areas-with-overlay
  [:div.wrapper-areas00
   [:div {:style {:grid-area "header"
                  :background-color "green"}}
    "abcde"]
   [:div {:style {:grid-area "content"}}
    lorem]
   [:div {:style {:grid-area "sidebar"}}
    lorem]
   [:div {:style {:grid-area "footer"
                  :background-color "#4B4B4B"}}
    "efgh"]
   [:div.overlay0]
   [:div.overlay1]
])








(defcard-rg repeat
  [:div.wrapper-repeat
   [:div.box]
   [:div.box.r1  1]
   [:div.box.r2  2]
   [:div.box.r3  3]
   [:div.box.r4  4]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box.r8  8]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]])


(defcard-rg repeat1
  [:div.wrapper-repeat1
   [:div.box]
   [:div.box.r01  1]
   [:div.box.r02  2]
   [:div.box.r03  3]
   [:div.box.r04  4]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box.r01-content lorem ]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]
   [:div.box]])



(defcard-rg density
  [:div.wrapper-auto-flow   
   [:div.box18 1]
   [:div.box18.wide :wide2]
   [:div.box18 3]
   [:div.box18 4]
   [:div.box18.tall :tall4]
   [:div.box18 5]
   [:div.box18 6]
   [:div.box18 7]
   [:div.box18 8]
   [:div.box18.wide :wide9]
   [:div.box18 10]
   [:div.box18 12]
   [:div.box18 13]
   [:div.box18.tall :tall13]
   [:div.box18.wide 14]
   [:div.box18 1]
   [:div.box18 1]
   [:div.box18 1]
   [:div.box18.tall :tall1]
   [:div.box18.wide 3]
   [:div.box18.wide 5]] 
)




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


(defcard-rg flexgrid
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
