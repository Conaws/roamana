(ns roamana.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [roamana.graphs]
            [roamana.zz]
            [roamana.zz2]
            [roamana.storage]
            [roamana.embed]
            [roamana.storage1]
            [roamana.gridview]
            [roamana.popover]
            [roamana.complete]
            [roamana.video]
            [reagent.session :as session]
            [roamana.core :as core])
  (:require-macros
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(defcard-rg first-card
  [:div>h1 "This is your first devcard!"])

(defcard-rg home-page-card
  [core/home-page])

(reagent/render [:div] (.getElementById js/document "app"))

;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards

