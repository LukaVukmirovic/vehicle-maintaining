(ns vehicle-maintaining.core
     (:require [clojure.xml :as xml])
     (:gen-class))


(comment
     (xml/parse "xml-data/vehicle.xml")

     (def expences (xml/parse "xml-data/xpenses.xml"))
     (get-in expences [:tag :month])

     (map (juxt :tag :content) [{:tag :da-mo, :attrs nil, :content ["202506"]} {:tag :expense, :attrs nil, :content ["80"]}]
     )                                                      ;;([:da-mo ["202506"]] [:expense ["80"]])

     (get-in [{:tag :da-mo, :attrs nil, :content ["202506"]} {:tag :expense, :attrs nil, :content ["80"]}] [1 :content 0]) ;; 80

     (map :content (get-in expences [:content]))
;;([{:tag :da-mo, :attrs nil, :content ["202504"]} {:tag :expense, :attrs nil, :content ["130"]}]
;; [{:tag :da-mo, :attrs nil, :content ["202505"]} {:tag :expense, :attrs nil, :content ["50"]}]
;; ...)


     (map #(get-in % [1 :content 0]) (map :content (get-in expences [:content])))
;;("130" "50" "80" "220" "60" "314" "150" "46" "67" "33" "64" "75" "130")

;; midje za test mozda - ubaciti u dependencies naznaciti da je vezano za test u dev profilu
;; github midje u redame ima uputstvo kako ga ubaciti
     )

(def service-types #{:small :big :battery :breaking-system :tires})

(defn element->map
     [element]
     (merge (:attrs element)
            (into {} (map (juxt :tag (comp first :content)) (:content element)))))

(defn ->vehicle [m]
     {:brand (:brand m)
      :model (:model m)
      :year (parse-long (:year m))
      :start-km (parse-long (:start-km m))})

(defn ->km-reading [m]
     {:id (parse-long (:id m))
      :date (:date m)
      :km (parse-long (:value m))})

(defn ->service [m]
     {:id   (parse-long (:id m))
      :type (keyword (:type m))
      :km   (parse-long (:km m))
      :date (:date m)})

(defn ->expense [m]
     {:id (parse-long (:id m))
      :month (:da-mo m)
      :amount (parse-long (:expense m))})

(defn xml-children [path]
     (map element->map (:content (xml/parse path))))

(defn load-db
     "Učitava sve XML u db mapu."
     [dir]
     {:vehicle  (->vehicle (element->map (xml/parse (str dir "/vehicle.xml"))))
      :km       (mapv ->km-reading (xml-children (str dir "/km.xml")))
      :services (mapv ->service    (xml-children (str dir "/services.xml")))
      :expenses (mapv ->expense    (xml-children (str dir "/xpenses.xml")))})

(defn -main [& args] (println "Vechile maintaining"))