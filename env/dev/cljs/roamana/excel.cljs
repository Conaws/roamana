(ns roamana.excel
  (:require [reagent.core :as r :refer [atom]]
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


(def mdb (atom {:size 4
               :text "Hillary vs Trump"
               :rows [:A  :B]
               :columns [:X :Y :Z ]}))




(deftest colgen
  (testing "interleave"
            (is (= 1 1))))



(defn grid0 [db]
  (fn []
    [:div {:style {:display "grid"
                   :grid-template-rows "[Row-Headers] auto [A] auto [B] auto [end]"
                   :grid-template-columns "[Column-Headers] 20px [X] auto [Y] auto [end]"
                   :grid-row-gap "10px"
                   :grid-column-gap "10px"
                   :width "500px"
                   :height "200px"}}
     [:div {:style
            {:background-color "blue"
             :grid-row "Row-Headers"
             :grid-column "Column-Headers / end"}}
      :a]
     [:div {:style
            {:background-color "green"
             :grid-row "Row-Headers / end"
             :grid-column "Column-Headers"}}
      :b]
     [:div.box {:style {:grid-row "B"
                        :grid-column "X / span 1" }}
      "starterB"]
     [:div.box {:style {:grid-row "A"
                        :grid-column "Y / span 1"}}
      "starterA"] ]))


(defcard-rg gridtest0
  [grid0])



#_(deftest  specter-transforms
  (testing "Specter"
    (is 
     (= 
      [1 :x 2 :y]
      (setval [ALL LAST] :y [1 :x 2])))))

(defn grid1 [mdb]
  (fn []
    [:div {:style {:display "grid"
                   :grid-template-rows "[Row-Headers] auto [A] auto [B] auto [end]"
                   :grid-template-columns "[Column-Headers] auto [X] auto [Y] auto [end]"
                   :grid-row-gap "10px"
                   :grid-column-gap "10px"
                   ;:align-items "center"
                   :width "500px"
                   :height "200px"}}
     [:div {:style
            {:background-color "white"
             :grid-row "Row-Headers"
             :grid-column "Column-Headers / end"}}
      
]
     [:div {:style
            {:background-color "green"
             :opacity "0.2"
             :grid-row "Row-Headers / end"
             :grid-column "Column-Headers"}}
      :b]

     (for [row (:rows @mdb)]
       [:div
        {:style {:grid-row row
                 :grid-column "Column-Headers"
                 :display "flex"
                 :align-items "center"
                 :justify-content "center"}}
        row])
     (for [c (:columns @mdb)]
       [:div
        {:style {:grid-row "Row-Headers"
                 :grid-column c
                 :display "flex"
                 :align-items "center"
                 :justify-content "center"}}
        c])

     (for [row (:rows @mdb)
           column (:columns @mdb)]
       [:button {:style {:grid-row row
                         :grid-column column}} 
        (str row column)])
      ]))


(defn row-header [row]
  (fn [row]
   ^{:key row} [:div
       {:style {:grid-row row
                :grid-column "Column-Headers"
                :display "flex"
                :align-items "center"
                :justify-content "center"}}
       row]))


(defn column-header [c]
  (fn [c]
    ^{:key (str c "header")} 
    [:div
        {:style {:grid-row "Row-Headers"
                 :grid-column (str c)
                 :display "flex"
                 :align-items "center"
                 :justify-content "center"}}
        c]))


(def header-styles 
  {:row-header  {:style
                 {:background-color "black"
                  :opacity "0.1"
                  :grid-row "Row-Headers"
                  :grid-column "Column-Headers / end"}}
   :column-header   {:style
                     {:background-color "green"
                      :opacity "0.2"
                      :grid-row "Row-Headers / end"
                      :grid-column "Column-Headers"}}})

(defn grid2 [mdb]
  (fn [mdb]
    [:div {:style {:display "grid"
                   :grid-template-rows "[Row-Headers] auto [A] auto [B] auto [end]"
                   :grid-template-columns "[Column-Headers] auto [X] auto [Y] auto [end]"
                   :grid-row-gap "10px"
                   :grid-column-gap "10px"
                   ;:align-items "center"
                   :width "500px"
                   :height "200px"}}
     
     [:div (:row-header header-styles)]
     [:div (:column-header header-styles)]

     (for [row (:columns @mdb)]
       [column-header row])
     (for [row (:rows @mdb)]
       [row-header row])
   ;  [column-headers mdb]
     (for [row (:rows @mdb)
           column (:columns @mdb)]
       [:button {:style {:grid-row row
                         :grid-column column}} 
        (str row column)])
      ]))


(interleave [1 2 3] [1.5 2.5])

(defn make-grid-string [start end space str-vec]
    (->> str-vec
         (map #(str "[" % "]"))
         (cons start)
         vec
         (#(conj % end))
         (clojure.string/join space)))

(def make-column (partial make-grid-string "[Column-Headers]" "[end]" " auto "))
(def make-row (partial make-grid-string "[Row-Headers]" "[end]" " auto "))



(def db2 (atom {:columns ["A" "B" "C"]
                :rows ["X" "Y" "Zd-is-Last"]}))


(deftest make-column-string
  (testing "makecolumn"
    (is (= "[Column-Headers] auto [X] auto [Y] auto [end]") 
        (make-column ["X" "Y"]))
    (is (= [[:A :1] [:A :2]
            [:B :1]  [:B :2]] (for [a [:A :B]
                                    n [:1 :2]]
                                [a n])))))



(defn grid3 [mdb]
  (fn [mdb]
    (let [rs (:rows @mdb)
          cls (:columns @mdb)]
      [:div {:style {:display "grid"
                     :grid-template-rows (make-row rs)
                     :grid-template-columns (make-column cls)
                     :grid-row-gap "20px"
                     :grid-column-gap "20px"
                                        ;:align-items "center"
                     :width "500px"
                     :height "500px"}}
       (for [c cls]
         [:button {:style
                   {:grid-row "end"
                    :grid-column c}}
          :A])
(for [r rs]
         [:button {:style
                   {:grid-row r
                    :grid-column "end"}}
          :A])
       [:div (:row-header header-styles)]
       [:div (:column-header header-styles)]
                                        ;(pr-str (type (make-column (:columns @mdb))))

       (for [c cls]
         [column-header c])
       (for [row rs]
         [row-header row])
                                        ;  [column-headers mdb]
       (for [r rs
             c cls]
         ^{:key (str r c)} [:button {:style {:grid-column c
                                             :grid-row r}}
                            #_{:style {:grid-row (str r)
                                       :grid-column (str c " / span 1")}} 
                            (str r " " c)])
       ])))

(defcard-rg gridtest3
  [grid3 db2]
  db2
  {})


(deftest make-rows
  (testing "rows"
    (is (s/valid? integer? (make-row (map str  (range 20)))))))





(defn empty-grid [mdb]
  (fn [mdb]
    (let [rs (map #(str "Row" %) (range 20))
          cls (map #(str "Column" %) (range 20))]
      [:div {:style {:display "grid"
                     :grid-template-rows (make-row rs)
                     :grid-template-columns (make-column cls)
                    ; :grid-row-gap "2px"
                    ; :grid-column-gap "2px"
                                        ;:align-items "center"
                     :width "700px"
                     :height "700px"}}
       #_(for [c cls]
         [:button {:style
                   {:grid-row "end"
                    :grid-column c}}
          :A])
#_(for [r rs]
         [:button {:style
                   {:grid-row r
                    :grid-column "end"}}
          :A])
       [:div (:row-header header-styles)]
       [:div (:column-header header-styles)]
                                        ;(pr-str (type (make-column (:columns @mdb))))
#_[:h1 rs]
       #_(for [c cls]
         [column-header c])
       #_(for [row rs]
         [row-header row])
                                        ;  [column-headers mdb]
       (for [r rs
             c cls]
         ^{:key (str r c)} 
         [:div {:style {:grid-column c
                        :grid-row r
                        :min-width "20px"
                        :min-height "20px"
                       
                        :border "1px solid white"
                        :border-width "1px 1px 0px 0px "
                        :background-color "#D8d8d8"}}
                        " "    
                            #_(str r " " c)])
       ])))






(defcard-rg empty-grid-test
  [empty-grid db2]
  db2
  {})




(defn grid [mdb]
  (fn [mdb]
    (let [rs (map #(str "Row" %) (range 20))
          cls (map #(str "Column" %) (range 20))]
      [:div {:on-mouse-up (fn [e]
                                 (swap! mdb #(setval :down false %)))
             :style {:display "grid"
                     :grid-template-rows (make-row rs)
                     :grid-template-columns (make-column cls)
                    ; :grid-row-gap "2px"
                    ; :grid-column-gap "2px"
                                        ;:align-items "center"
                     :width "700px"
                     :height "700px"}}
      
       [:div (:row-header header-styles)]
       [:div (:column-header header-styles)]
                                        ;(pr-str (type (make-column (:columns @mdb))))

       (for [comp (:components @mdb)]
         [:div
          {:style {:grid-column (str (:col comp) " / span 2")
                   :grid-row (:row  comp)
                   :overflow "hidden"
               ;    :max-width "20px"
               ;    :max-height "20px"
                   :background-color "Green"
                   :z-index 5}
           }
          "Here"])
       

       (for [r rs
             c cls]
         ^{:key (str r c)} 
         [:div {:style {:grid-column c
                        :grid-row r
                        :min-width "20px"
                        :min-height "20px"
                        :border "1px solid white"
                        :border-width "1px 1px 0px 0px "
                        :background-color "#D8d8d8"}
                :on-mouse-enter #(do 
                             (let [a {:row r
                                      :col c}]
                               (if (:down  @mdb)
                                 (swap! mdb (fn [m] (transform :components
                                                               (fn [vector-of-comps]
                                                                 (if vector-of-comps
                                                                   (conj vector-of-comps a)
                                                                   [a])) m)))))
                             )
                :on-mouse-down (fn [e]
                                 (swap! mdb #(setval :down true %)))
                
                :on-click (fn [e] 
                            (do 
                              (let [a {:row r
                                       :col c}]
                               
                                (swap! mdb (fn [m] (transform :components
                                                              (fn [vector-of-comps]
                                                                (if vector-of-comps
                                                                  (conj vector-of-comps a)
                                                                  [a])) m)))))
                             )}
                        " "    
                            #_(str r " " c)])
       ])))


(def db3 (atom {:components [{:row "Row5"
                              :col "Column5"}]}))


(defcard-rg click-grid
  [grid db3]
  db2
  {})
