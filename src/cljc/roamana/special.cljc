(ns roamana.special
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.spec :as s]
    ))


(def link-regex #"(?:\[\[)([^\[\]]+\S)\]\]") 


(s/def ::link (s/and string?  #(re-matches link-regex %)))


(s/def ::string string?)




(defn link-gen []
  (->> (s/gen integer?)
       ))


(clojure.pprint/pprint
  (+ 1 2))

;(pprint (gen/sample (gen/choose 5 9)))

;(pprint (macroexpand (s/exercise ::string 10 {::string link-gen})))



