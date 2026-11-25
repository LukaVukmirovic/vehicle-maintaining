(ns vehicle-maintaining.core-test
  (:require [midje.sweet :refer :all]
            [vehicle-maintaining.core :as core]))

(def sample-db
  {:vehicle  {:brand "Toyota" :model "Corolla" :year 2015 :start-km 100000}
   :km       [{:id 1 :date "20250101" :km 100000}
              {:id 2 :date "20250601" :km 108000}]
   :services [{:id 1 :type :small :km 100000 :date "20250101" :cost 80}
              {:id 2 :type :tires :km 105000 :date "20250301" :cost 200}
              {:id 3 :type :small :km 110000 :date "20250901"}]
   :expenses [{:id 1 :month "202511" :amount 100}
              {:id 2 :month "202512" :amount 50}
              {:id 3 :month "202601" :amount 30}]})

(def services (:services sample-db))
(def expenses (:expenses sample-db))

(facts "XML element -> mapa"
       (core/element->map
         {:tag     :km
          :attrs   {:id "1"}
          :content [{:tag :date  :attrs nil :content ["20250416"]}
                    {:tag :value :attrs nil :content ["136000"]}]})
       => {:id "1" :date "20250416" :value "136000"})

(facts "text -> string"
       (core/->vehicle {:brand "Toyota" :model "Corolla" :year "2015" :start-km "136000"})
       => {:brand "Toyota" :model "Corolla" :year 2015 :start-km 136000}

       (core/->service {:id "2" :type "tires" :km "139000" :date "20250712"})
       => {:id 2 :type :tires :km 139000 :date "20250712"}

       (core/->expense {:id "1" :da-mo "202504" :expense "130"})
       => {:id 1 :month "202504" :amount 130})

(facts "CSV"
       (fact "zaglavlje se preskace, red se deli na polja"
             (core/parse-csv "a,b\n1,2\n3,4") => [["1" "2"] ["3" "4"]])
       (fact "delovi se dele po ;"
             (core/->service-cost ["1" "85" "ulje;filter"])
             => {:service-id 1 :cost 85 :parts ["ulje" "filter"]}))

(facts "index-by i attach-costs"
       (core/index-by :id [{:id 1 :a 1} {:id 2}]) => {1 {:id 1 :a 1} 2 {:id 2}}
       (fact "servis bez reda u CSV-u ostaje nepromenjen"
             (core/attach-costs [{:id 1} {:id 2}]
                                [{:service-id 1 :cost 85 :parts ["ulje"]}])
             => [{:id 1 :cost 85 :parts ["ulje"]} {:id 2}]))

(tabular "current-km uzima najveci broj"
         (fact (core/current-km ?kms) => ?expected)
         ?kms     ?expected
         [1 5 3]  5
         [7]      7
         []       0)

(facts "km-driven"
       (fact "servis na 110000 je veci od poslednjeg ocitavanja (108000)"
             (core/current-km (core/all-kms sample-db)) => 110000
             (core/km-driven sample-db) => 10000))

(facts "troskovi"
       (core/total-expenses expenses) => 180
       (core/average-expense expenses) => 60
       (fact "prosek je tacan razlomak (Ratio)"
             (core/average-expense [{:amount 1} {:amount 2}]) => 3/2
             (double (core/average-expense [{:amount 1} {:amount 2}])) => (roughly 1.5))
       (fact "prazna lista ne deli nulom"
             (core/average-expense []) => 0))

(facts "troskovi po godini i top"
       (core/total-expenses (core/year-expenses expenses "2025")) => 150
       (core/total-expenses (core/year-expenses expenses "2026")) => 30
       (map :amount (core/top-expenses expenses 2)) => [100 50])

(facts "cene servisa"
       (fact "servis bez cene se broji kao 0"
             (core/total-service-cost services) => 280)
       (core/total-cost sample-db) => 460)

(facts "upiti nad servisima"
       (core/last-service services) => (contains {:id 3})
       (map :id (core/types-services services :small)) => (just [1 3])
       (core/had-service? services :tires) => true
       (core/had-service? services :battery) => false
       (core/small-service? (first services)) => truthy
       (filter (complement core/small-service?) services) => (just [(contains {:type :tires})]))

(fact "next-small"
      (take 3 (core/next-small services :small 10000)) => [120000 130000 140000])

(facts "next-id"
       (core/next-id services) => 4
       (core/next-id []) => 1)

(facts "add-service vraca novi db"
       (let [db2 (core/add-service sample-db {:type :battery :km 111000 :date "20251001"})
             db3 (core/add-service sample-db {:type :tires :km 112000 :date "20251101"} "zimske")]
         (count (:services db2)) => 4
         (last (:services db2)) => {:id 4 :type :battery :km 111000 :date "20251001"}
         (last (:services db3)) => (contains {:note "zimske"})
         (fact "originalni db nije promenjen"
               (count (:services sample-db)) => 3)))

(facts "add-expense"
       (let [db2 (core/add-expense sample-db "202602" 70)]
         (last (:expenses db2)) => {:id 4 :month "202602" :amount 70}
         (core/total-expenses (:expenses db2)) => 250
         (core/total-expenses (:expenses sample-db)) => 180))

(fact "vehicle-label"
      (core/vehicle-label (:vehicle sample-db)) => "Toyota Corolla (2015)")

(facts "load-db nad app-data"
       (let [db (core/load-db "app-data")]
         (get-in db [:vehicle :brand]) => "Toyota"
         (:services db) => (n-of map? 5)
         (fact "svaki tip servisa iz XML-a postoji u service-types"
               (map :type (:services db)) => (has every? core/service-types))))