(ns roamana.video
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [datascript.core :as d]
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

(re-frame.utils/set-loggers! {:warn #(js/console.log "")})





(defn player [url]
  (let [u  (if )])
  [:iframe
   {:src url
    :height 400
    :width 400
}])


(def hammock  "https://www.youtube.com/embed/f84n5oFoZBc")

(def trigBX  "https://www.youtube.com/watch?v=rDE1GBUjTUM")



(re-find trigBX #"watch?")


(defcard-rg player
  [player hammock])



;<iframe id="ytplayer" type="text/html" width="640" height="390"
;  src="https://www.youtube.com/embed/m7lc1uvf-ve?autoplay=1&origin=http://example.com;"
;  frameborder="0"></iframe>
