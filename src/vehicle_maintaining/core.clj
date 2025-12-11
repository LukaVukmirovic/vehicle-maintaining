(ns vehicle-maintaining.core)
(require '[clojure.xml :as xml])

(xml/parse "xml-data/vehicle.xml")

(def vechile data)

(def expences (xml/parse "xml-data/xpenses.xml"))
(get-in expences [:tag :month])

(->> (:content expences)
     (map :content)
     (map (map :content)))

(map (juxt :tag :content) [{:tag :da-mo, :attrs nil, :content ["202506"]} {:tag :expense, :attrs nil, :content ["80"]}]
     )                                                      ;;([:da-mo ["202506"]] [:expense ["80"]])

(get-in [{:tag :da-mo, :attrs nil, :content ["202506"]} {:tag :expense, :attrs nil, :content ["80"]}] [1 :content 0]) ;; 80

(map :content (get-in expences [:content]))
;;([{:tag :da-mo, :attrs nil, :content ["202504"]} {:tag :expense, :attrs nil, :content ["130"]}]
;; [{:tag :da-mo, :attrs nil, :content ["202505"]} {:tag :expense, :attrs nil, :content ["50"]}]
;; ...)


(map #(get-in % [1 :content 0]) (map :content (get-in expences [:content])))
;;("130" "50" "80" "220" "60" "314" "150" "46" "67" "33" "64" "75" "130")