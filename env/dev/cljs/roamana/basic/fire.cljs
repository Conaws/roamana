(ns roamana.basic.fire
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.db :refer [app-db]]
            [re-frame.core :refer [subscribe dispatch register-handler register-sub]]
            [posh.core :refer [posh!] :as posh]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [com.rpl.specter  :refer [ALL STAY MAP-VALS LAST
                                      stay-then-continue 
                                      if-path END cond-path
                                      must pred keypath
                                      collect-one comp-paths] :as sp]
            [cljs.spec  :as s]
            [cljsjs.firebase]
            [roamana.util :refer [c]
             :refer-macros [s!]]
            [clojure.set :as set]
            [clojure.string :as str]
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


(defonce user
  (atom nil))



(defn db-ref [path]
  (.ref (js/firebase.database) (str/join "/" path)))


(defn save [path value]
  (.set (db-ref path) value))


(defn load [path]
  (.once
   (db-ref path)
   "value"
   (fn recieved-db [snapshot]
     (prn "Got:" (.val snapshot)))))

#_(defn on [path f]
  (let [ref (db-ref path)
        a (reagent/atom nil)]
    (.on ref "value" (fn [x]
                       (reset! a (.val x))))
    (reagent/create-class
     {:display-name "listener"
      :component-will-unmount
      (fn will-unmount-listener [this]
        (.off ref))
      :reagent-render 
      (fn render-listener [args]
        (into [f a] args))})))


(comment 
  (save ["test"] "hello world")
  (load ["test"]))


#_(defn testfire []
  [on ["test"]
   (fn [a]
     [:div @a])])


(defcard-rg save-card
  [:form
   {:on-submit
    (fn [e]
      (.preventDefault e)
      (save ["test"] (forms/getValueByName (.-target e) "test")))}
   [:div "Type a message and save it to Firebase"]
   [:input
    {:type "text"
     :name "test"}]
   [:input
    {:type "submit"}]])




(defn once
  "Retreives the firebase state at path as a ratom that will be set when the state arrives."
  [path]
  (let [a (reagent/atom nil)]
    (.once
      (db-ref path)
      "value"
      (fn received-db [snapshot]
        (reset! a (.val snapshot))))
    a))


(defcard-rg once-card
  "This card will not update on changes, only when you refresh the page."
  (fn []
    (let [a (once ["test"])]
      (fn []
        [:div @a]))))



(defn on [path component]
  (let [ref (db-ref path)
        a (atom nil)]
    (.on ref "value"
         (fn [x]
           (reset! a (.val x))))
    (reagent/create-class
     {:display-name "listener"
      :component-will-unmount
      (fn will-unmount-listener [this]
        (.off ref))
      :reagent-render 
      (fn 
        [path component & args]
        (into [component a] args))})))



(defcard-rg on-cad
  [:div
   [on ["test"]
    (fn [a]
      [:div @a])]])


(defn sign-in []
  (.signInWithRedirect
   (js/firebase.auth.)
   (js/firebase.auth.GoogleAuthProvider.)))


(defn sign-in-with-popup []
  (do 
    (.signInWithPopup
     (js/firebase.auth.)
     (js/firebase.auth.GoogleAuthProvider.))))


(defn sign-in-with-email []
  (.createUserWithEmailAndPassword
   (js/firebase.auth.)
   "conor@firebase.com"
   "1234567"))


(defn sign-out []
  (do
    (.signOut (js/firebase.auth))
    (reset! user nil)))


(defn on-auth []
  (.onAuthStateChanged
   (js/firebase.auth)
   (fn auth-state-changed [user-obj]
     (let [uid (.-uid user-obj)
           display-name (.-displayName user-obj)
           photo-url (.-photoURL user-obj)]
       (if uid
         (do
           (save ["users" uid "settings"]
                 #js {:photo-url photo-url
                      :display-name display-name
                      :uid uid})
           (reset! user
                   {:photo-url photo-url
                    :display-name display-name
                    :uid uid}))
         (when @user
           (reset! user nil)))))
   (fn auth-error [error]
     (js/console.log error))))



(defn login-view []
  (fn []
    [:div
     [:button (c #(sign-out))
      "out"]
     (if-let [{:keys [photo-url  display-name]} @user]
       [:div
        [:button
         {:on-click #(sign-out)
          :title display-name 
          :background-image (str "url(" photo-url ")")
          :background-size "cover"
          :background-repeat "no-repeat"}
         "logout"]
        (pr-str @user)]
       [:div
        [:button
         {:on-click #(sign-in-with-email)}
         "login with email"]
        [:button
         {:on-click #(sign-in-with-popup)}
         "login with google"]])]))



(defcard-rg login-card
  [login-view]
  user
  {:inspect-data true
   :history true})



(defn init []
  (js/firebase.initializeApp
   #js {:apiKey "AIzaSyDEtDZa7Sikv7_-dFoh9N5EuEmGJqhyK9g"
        :authDomain "firescript-577a2.firebaseapp.com"
        :databaseURL "//firescript-577a2.firebaseio.com"
        :storageBucker ""
        })
  (on-auth))


(init)
