(ns logatom.graphs
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch]]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      collect-one comp-paths] :as sp]
            [logatom.logger :refer [conn]]
            [logatom.views :refer [main-view logmap todo-create] :as views]
            [reagent.session :as session]
            [goog.i18n.DateTimeFormat :as dtf]
            [logatom.core :as core])
  (:require-macros
   [com.rpl.specter.macros  :refer [select transform declarepath providepath]]
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))


(def format-map
  (let [f goog.i18n.DateTimeFormat.Format]
{:FULL_DATE (.-FULL_DATE f)
:FULL_DATETIME (.-FULL_DATETIME f)
:FULL_TIME (.-FULL_TIME f)
:LONG_DATE (.-LONG_DATE f)
:LONG_DATETIME (.-LONG_DATETIME f)
:LONG_TIME (.-LONG_TIME f)
:MEDIUM_DATE (.-MEDIUM_DATE f)
:MEDIUM_DATETIME (.-MEDIUM_DATETIME f)
:MEDIUM_TIME (.-MEDIUM_TIME f)
:SHORT_DATE (.-SHORT_DATE f)
:SHORT_DATETIME (.-SHORT_DATETIME f)
 :SHORT_TIME (.-SHORT_TIME f)}))



(defn format-date*
  "Format a date using either the built-in goog.i18n.DateTimeFormat.Format enum
or a formatting string like \"dd MMMM yyyy\""
  [date-format date]
  (.format (goog.i18n.DateTimeFormat.
            (or (date-format format-map) date-format))
           (js/Date. date)))

(format-date* :SHORT_DATE (js/Date.))
(format-date* :MEDIUM_TIME (js/Date.))
(format-date* :SHORT_TIME (js/Date.))
(def short-date (partial format-date* :SHORT_DATETIME))
(def short-time (partial format-date* :SHORT_TIME))




(def samplelog (atom {:log {#inst "2016-06-18T20:04:19.909-00:00" {:datoms [[1 :todo/text "bom" 536870913 true]], :visible true}, #inst "2016-06-18T20:04:23.694-00:00" {:datoms [[2 :todo/text "bang" 536870914 true]], :visible true}, #inst "2016-06-18T20:04:31.345-00:00" {:datoms [[3 :todo/text "c" 536870915 true]], :visible true}}}))



(select [MAP-VALS MAP-VALS] (:log @app-db))


(declare svg1)

(defn newmain [conn]
  (fn []
    [:div
     (pr-str @@conn)
     [todo-create conn]
     [svg1 conn]
     [main-view conn]]))


(defcard-rg mainview
  [newmain conn])
   
(select [:log MAP-VALS :datoms ALL (sp/srange 0 3)] @samplelog)



(defn svg1 [conn]
  (let [l (subscribe [:log])]
   (fn [conn]
     (into 
      [:svg {:view-box "0 0 15 15"
             :height 800
             :width 500}]
       (conj
        (for [[i [tx log]] (map-indexed vector (reverse (into (sorted-map) @l)))]
          [:g 
           [:text {:font-size 0.33
                   :x 3
                   :y i
                   :text-anchor "start"
                   :fill "blue"}
            (apply str (flatten (vector (short-time tx)
                                ":  "
                                (for [[e a v t t?] (:datoms log)]
                                  (str (if t?
                                         "ADDED"
                                         "REMOVED")
                                       " " 
                                       a 
                                       "  "
                                       v
                                       (if t?
                                         " to "
                                         " from ")
                                       e)))))]
           [:circle {:r 0.32
                     :cx 2
                     :cy i
                     :on-click #(dispatch [:toggle-log-visible tx conn])
                     :style {:cursor "pointer"}
                     :fill (if (:visible log)
                             "blue"
                             "red")}]])
        [:line {:x1 2 :x2 2 :y1 0 :y2 (dec (count @l))
                :stroke "blue"
                :stroke-width 0.15}])))))






(defcard-rg first-graph
    [svg1 conn])

