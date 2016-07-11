(ns roamana.keys
  (:require [keybind.core :as key]
            [re-frame.core :refer [dispatch]]))


#_(defn nav-keys [{:keys [left right down up]}]
    (key/bind! right ::right #(dispatch [::inc-depth]))
    (key/bind! left ::left #(dispatch [::dec-depth]))
    (key/bind! up ::up #(dispatch [::dec-cursor]))
    (key/bind! down ::down  #(dispatch [::inc-cursor])))

