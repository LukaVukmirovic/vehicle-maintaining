(ns vehicle-maintaining.core
     (:require [clojure.xml :as xml]
               [clojure.string :as str])
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

(def service-types #{:small :big :battery :braking-system :tires})

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

(defn parse-csv [text]
     (->> (str/split-lines text)
          (drop 1)
          (map #(str/split % #","))))

(defn ->service-cost [[id cost parts]]
     {:service-id (parse-long id)
      :cost (parse-long cost)
      :parts (str/split parts #";")})

(defn load-service-cost [path]
     (map ->service-cost (parse-csv (slurp path))))

(defn index-by [k coll]
     (into {} (map (juxt k identity) coll)))

(defn attach-costs [services costs]
     (let [by-id (index-by :service-id costs)]
          (mapv #(merge % (select-keys (get by-id (:id %)) [:cost :parts]))
                services)))

(defn load-db
     "Učitava sve XML u db mapu."
     [dir]
     {:vehicle  (->vehicle (element->map (xml/parse (str dir "/vehicle.xml"))))
      :km       (mapv ->km-reading (xml-children (str dir "/km.xml")))
      :services (attach-costs (mapv ->service    (xml-children (str dir "/services.xml")))
                              (load-service-cost (str dir "/service-cost.csv")))
      :expenses (mapv ->expense    (xml-children (str dir "/xpenses.xml")))})

(defn vehicle-label [{:keys [brand model year]}]
     (str brand " " model " (" year ")"))

(defn current-km
     [kms]
     (reduce max 0 kms))

(defn all-kms
     [{:keys [km services]}]
     (concat (map :km km) (map :km services)))

(defn km-driven
     [{:keys [vehicle] :as db}]
     (- (current-km (all-kms db)) (:start-km vehicle)))

(defn total-expenses [expenses]
     (reduce + 0 (map :amount expenses)))

(defn next-id [coll]
     (inc (reduce max 0 (map :id coll))))

(defn average-expense [expenses]
     (if (empty? expenses)
          0
          (/ (total-expenses expenses) (count expenses))))

(defn add-service
     ([db service] (add-service db service nil))
     ([db service note]
      (let [with-id (assoc service :id (next-id (:services db)))
            with-note (if note (assoc with-id :note note) with-id)]
           (update db :services conj with-note))))

(defn add-expense
     [db month amount]
     (update db :expenses conj {:id (next-id (:expenses db))
                                :month month
                                :amount amount}))

(defn last-service [services]
     (last (sort-by :km services)))

(defn top-expenses [expenses n]
     (->> expenses
          (sort-by :amount >)
          (take n)))

(defn year-expenses [expenses year]
     (filter #(str/starts-with? (:month %) year) expenses))

(defn had-service? [services t]
                   (boolean (some #(= t (:type %)) services)))

(defn total-service-cost [services]
     (apply + (map #(:cost % 0) services)))

(defn total-cost [{:keys [expenses services]}]
     (+ (total-expenses expenses) (total-service-cost services)))

(defn service-type? [t service]
     (= t (:type service)))

(def small-service? (partial service-type? :small))

(defn types-services [services t]
     (filter (partial service-type? t) services))

(def small-service-interval 10000)

(defn next-small [services t interval]
     (let [last-km (:km (last-service (types-services services t)))]
          (rest (iterate #(+ % interval) last-km))))

(defn -main [& args]
     (let [db (load-db "app-data")
           db (add-expense db "202605" 200)
           expenses (:expenses db)
           services (:services db)]
          (println "Vozilo:" (vehicle-label (:vehicle db)))
          (println "Predjeno km:" (km-driven db))
          (println "Ukupni troskovi:" (total-expenses expenses))
          (println "Prosecan mesecni trosak:"
                   (format "%.2f" (double (average-expense expenses))))
          (let [{:keys [type km]} (last-service services)]
               (println "Poslednji servis:" type km "km"))
          (println "Mali servisi:" (count (types-services services :small)))
          (println "Troskovi 2025:" (total-expenses (year-expenses expenses "2025")))
          (println "Troskovi 2026:" (total-expenses (year-expenses expenses "2026")))
          (println "top 3 troska")
          (doseq [{:keys [month amount]} (top-expenses expenses 3)]
               (println " " month amount))
          (println "Menjan akumulator:" (had-service? services :battery))
          (println "zasebno troskovi servisa:" (total-service-cost services))
          (println "ukupno mesecni + servisi" (total-cost db))
          (println "broj servisa koji nisu mali:" (count (filter (complement small-service?) services)))
          (println "sledeci mali servis na:" (take 1 (next-small services :small small-service-interval)))
          (println "delovi na servisu 3:" (str/join ", " (:parts (get (index-by :id services) 3))))))

;;Vozilo: Toyota Corolla (2015)
;;Predjeno km: 15000
;;Ukupni troskovi: 1619
;;Prosecan mesecni trosak: 115.64
;;Poslednji servis: :small 151000 km
;;Mali servisi: 2
;;Troskovi 2025: 1117
;;Troskovi 2026: 502
;;top 3 troska
;;  202509 314
;;  202507 220
;;  202605 200
;;Menjan akumulator: true
;;zasebno troskovi servisa: 705
;;ukupno mesecni + servisi 2324
;;broj servisa koji nisu mali: 3
;;sledeci mali servis na: (161000)
;;delovi na servisu 3: plocice, diskovi