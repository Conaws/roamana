(ns roamana.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [roamana.undoable]
            [roamana.zz]
     ;       [roamana.zz2]
            [roamana.zz3]
            [roamana.storage]
            [roamana.basic.gridview]
            [roamana.basic.flex]
            [roamana.popover]
            [roamana.search]
            [roamana.links]
            [roamana.logic]
            [roamana.parser]
            [roamana.query]
            [roamana.excel]
            [roamana.outline]
            [roamana.video]
            [roamana.basic.kanban]
            [roamana.basic.multiembed]
            [roamana.basic.ents]
            [roamana.basic.fire :as firebase]
            [roamana.svg.foreign]
            [roamana.basic.recomplete]
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


(firebase/init)
;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards

