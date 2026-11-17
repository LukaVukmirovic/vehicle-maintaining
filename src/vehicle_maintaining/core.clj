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

(defn vehicle-label [{:keys [brand model year]}]
     (str brand " " model " (" year ")"))

(defn current-km
     [km-readings]
     (loop [[r & more] km-readings
            best 0]
          (if (nil? r)
               best
               (recur more (max best (:km r))))))

(defn km-driven
     [{:keys [vehicle km]}]
     (- (current-km km) (:start-km vehicle)))

(defn total-expenses [expenses]
     (loop [[e & more] expenses
            sum 0]
          (if (nil? e)
               sum
               (recur more (+ sum (:amount e))))))

(defn average-expense [expenses]
     (if (empty? expenses)
          0
          (/ (total-expenses expenses) (count expenses))))

(defn add-service
     ([db service] (add-service db service nil))
     ([db service note]
      (let [with-id (assoc service :id (inc (count (:services db))))
            with-note (if note (assoc with-id :note note) with-id)]
           (update db :services conj with-note))))

(defn add-expense
     [db month amount]
     (update db :expenses conj {:id (inc (count (:expenses db)))
                                :month month
                                :amount amount}))

(defn -main [& args]
     (let [db (load-db "xml-data")
           db (add-expense db "202605" 200)
           expenses (:expenses db)]
          (println "Vozilo:" (vehicle-label (:vehicle db)))
          (println "Predjeno km:" (km-driven db))
          (println "Ukupni troskovi:" (total-expenses expenses))
          (println "Prosecan mesecni trosak:"
                   (format "%.2f" (double (average-expense expenses))))))

;;ispis pre dodavanja novog troska
;;Vozilo: Toyota Corolla (2015)
;;Predjeno km: 13447
;;Ukupni troskovi: 1419
;;Prosecan mesecni trosak: 109.15

;;ispis posle dodavanja novog troska
;;Vozilo: Toyota Corolla (2015)
;;Predjeno km: 13447
;;Ukupni troskovi: 1619
;;Prosecan mesecni trosak: 115.64