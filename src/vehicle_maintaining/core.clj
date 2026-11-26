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

(comment (defn -main [& args]
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
         )

(defn p-summary [{:keys [vehicle expenses services] :as db}]
  (println "Vozilo:" (vehicle-label (:vehicle db)))
  (println "Predjeno km:" (km-driven db))
  (println "Ukupni troskovi:" (total-expenses expenses))
  (println "Prosecan mesecni trosak:"
           (format "%.2f" (double (average-expense expenses))))
  (let [{:keys [type km]} (last-service services)]
    (println "Poslednji servis:" type km "km"))
  (println "Troskovi servisa:" (total-service-cost services)))

(defn p-services [services]
  (doseq [{:keys [id type km date cost parts]} services]
    (println (format "%2d  %-15s %7d km  %s  %5s  %s"
                     id (name type) km date (or cost "-") (str/join ", " parts))))
  (println "Malih:" (count (types-services services :small))
           "| ostalih:" (count (filter (complement small-service?) services))))

(defn p-expenses [expenses]
  (doseq [year (sort (distinct (map #(subs (:month %) 0 4) expenses)))]
    (println year ":" (total-expenses (year-expenses expenses year))))
  (println "Top 3:")
  (doseq [{:keys [month amount]} (top-expenses expenses 3)]
    (println " " month amount)))

(defn p-next-service [db]
  (let [nexts   (take 3 (next-small (:services db) :small small-service-interval))
        current (current-km (all-kms db))]
    (println "Na km:" nexts)
    (println "Preostalo do prvog:" (- (first nexts) current) "km")))


(defn check-expense-input [month amount-str]
  (let [amount (parse-long amount-str)]
    (cond
      (not (re-matches #"\d{6}" month)) {:error "format mora biti YYYYMM"}
      (nil? amount) {:error "mora biti ceo broj"}
      (neg? amount) {:error "ne sme biti negativna vrednost"}
      :else {:month month :amount amount})))

(defn prompt [text]
  (print text)
  (flush)
  (str/trim (or (read-line) "0")))

(defn input-add-expense [db]
                   (let [result (check-expense-input (prompt "mesec (yyyymm):")
                                                     (prompt "iznos:"))]
                     (if-let [error (:error result)]
                       (do (println "greska: " error) db)
                       (let [db2 (add-expense db (:month result) (:amount result))]
                         (println "trosak dodat. ukupno: " (total-expenses (:expenses db2))) db2))))

(defn menu []
  (println "1) Pregled")
  (println "2) Servisi")
  (println "3) Troskovi")
  (println "4) Dodaj trosak")
  (println "5) Sledeci mali servis")
  (println "0) Izlaz"))

(defn run [initial-db]
  (loop [db initial-db]
    (menu)
    (case (prompt "izbor:")
      "1" (do (p-summary db) (recur db))
      "2" (do (p-services (:services db)) (recur db))
      "3" (do (p-expenses (:expenses db)) (recur db))
      "4" (recur (input-add-expense db))
      "5" (do (p-next-service db) (recur db))
      "0" (println "Exit")
      (do (println "nepoznata opcija") (recur db))
      )))

(defn -main [& args]
  (run (load-db "app-data")))

;;"/Applications/IntelliJ IDEA.app/Contents/jbr/Contents/Home/bin/java" -Dfile.encoding=UTF-8 -XX:-OmitStackTraceInFastThrow -Dclojure.compile.path=/Users/lukavukmirovic/IdeaProjects/vehicle-maintaining/target/classes -Dvehicle-maintaining.version=0.1.0-SNAPSHOT -Dclojure.debug=false "-javaagent:/Applications/IntelliJ IDEA.app/Contents/lib/idea_rt.jar=56675" -Dsun.stdout.encoding=UTF-8 -Dsun.stderr.encoding=UTF-8 -classpath /Users/lukavukmirovic/IdeaProjects/vehicle-maintaining/test:/Users/lukavukmirovic/IdeaProjects/vehicle-maintaining/src:/Users/lukavukmirovic/IdeaProjects/vehicle-maintaining/dev-resources:/Users/lukavukmirovic/IdeaProjects/vehicle-maintaining/resources:/Users/lukavukmirovic/IdeaProjects/vehicle-maintaining/target/classes:/Users/lukavukmirovic/.m2/repository/org/nrepl/incomplete/0.1.0/incomplete-0.1.0.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/core.rrb-vector/0.1.2/core.rrb-vector-0.1.2.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/test.check/1.1.1/test.check-1.1.1.jar:/Users/lukavukmirovic/.m2/repository/fipp/fipp/0.6.26/fipp-0.6.26.jar:/Users/lukavukmirovic/.m2/repository/org/tcrawley/dynapath/1.1.0/dynapath-1.1.0.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/clojure/1.12.2/clojure-1.12.2.jar:/Users/lukavukmirovic/.m2/repository/org/clojars/brenton/google-diff-match-patch/0.1/google-diff-match-patch-0.1.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/tools.macro/0.1.5/tools.macro-0.1.5.jar:/Users/lukavukmirovic/.m2/repository/potemkin/potemkin/0.4.5/potemkin-0.4.5.jar:/Users/lukavukmirovic/.m2/repository/mvxcvi/puget/1.3.4/puget-1.3.4.jar:/Users/lukavukmirovic/.m2/repository/marick/suchwow/6.0.3/suchwow-6.0.3.jar:/Users/lukavukmirovic/.m2/repository/nrepl/nrepl/1.0.0/nrepl-1.0.0.jar:/Users/lukavukmirovic/.m2/repository/environ/environ/1.2.0/environ-1.2.0.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar:/Users/lukavukmirovic/.m2/repository/commons-codec/commons-codec/1.15/commons-codec-1.15.jar:/Users/lukavukmirovic/.m2/repository/joda-time/joda-time/2.10/joda-time-2.10.jar:/Users/lukavukmirovic/.m2/repository/riddley/riddley/0.1.12/riddley-0.1.12.jar:/Users/lukavukmirovic/.m2/repository/io/aviso/pretty/1.4.4/pretty-1.4.4.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/java.classpath/1.0.0/java.classpath-1.0.0.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/tools.reader/1.3.6/tools.reader-1.3.6.jar:/Users/lukavukmirovic/.m2/repository/clj-tuple/clj-tuple/0.2.2/clj-tuple-0.2.2.jar:/Users/lukavukmirovic/.m2/repository/colorize/colorize/0.1.1/colorize-0.1.1.jar:/Users/lukavukmirovic/.m2/repository/clj-time/clj-time/0.15.2/clj-time-0.15.2.jar:/Users/lukavukmirovic/.m2/repository/mvxcvi/arrangement/2.1.0/arrangement-2.1.0.jar:/Users/lukavukmirovic/.m2/repository/flare/flare/0.2.9/flare-0.2.9.jar:/Users/lukavukmirovic/.m2/repository/com/rpl/specter/1.1.3/specter-1.1.3.jar:/Users/lukavukmirovic/.m2/repository/midje/midje/1.10.10/midje-1.10.10.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/math.combinatorics/0.2.0/math.combinatorics-0.2.0.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/tools.namespace/1.4.4/tools.namespace-1.4.4.jar:/Users/lukavukmirovic/.m2/repository/org/clojure/core.unify/0.5.7/core.unify-0.5.7.jar clojure.main -i /private/var/folders/61/3m20_x_93xs4bwm2wzyrr2nh0000gn/T/form-init18166686249565348433.clj
;;1) Pregled
;;2) Servisi
;;3) Troskovi
;;4) Dodaj trosak
;;5) Sledeci mali servis
;;0) Izlaz

;;izbor:1
;;Vozilo: Toyota Corolla (2015)
;;Predjeno km: 15000
;;Ukupni troskovi: 1419
;;Prosecan mesecni trosak: 109.15
;;Poslednji servis: :small 151000 km
;;Troskovi servisa: 705
;;1) Pregled
;;2) Servisi
;;3) Troskovi
;;4) Dodaj trosak
;;5) Sledeci mali servis
;;0) Izlaz

;;izbor:2
;; 1  small            136000 km  20250416     85  ulje, filter ulja
;; 2  tires            139000 km  20250712    240  4 gume
;; 3  braking-system   144000 km  20250926    180  plocice, diskovi
;; 4  battery          149000 km  20260107    110  akumulator
;; 5  small            151000 km  20260421     90  ulje, filter ulja, filter vazduha
;;Malih: 2 | ostalih: 3
;;1) Pregled
;;2) Servisi
;;3) Troskovi
;;4) Dodaj trosak
;;5) Sledeci mali servis
;;0) Izlaz

;;izbor:3
;;2025 : 1117
;;2026 : 302
;;Top 3:
;;  202509 314
;;  202507 220
;;  202510 150
;;1) Pregled
;;2) Servisi
;;3) Troskovi
;;4) Dodaj trosak
;;5) Sledeci mali servis
;;0) Izlaz

;;izbor:4
;;mesec (yyyymm):202504
;;iznos:111
;;trosak dodat. ukupno:  1530
;;1) Pregled
;;2) Servisi
;;3) Troskovi
;;4) Dodaj trosak
;;5) Sledeci mali servis
;;0) Izlaz

;;izbor:3
;;2025 : 1228
;;2026 : 302
;;Top 3:
;;  202509 314
;;  202507 220
;;  202510 150
;;1) Pregled
;;2) Servisi
;;3) Troskovi
;;4) Dodaj trosak
;;5) Sledeci mali servis
;;0) Izlaz

;;izbor:5
;;Na km: (161000 171000 181000)
;;Preostalo do prvog: 10000 km
;;1) Pregled
;;2) Servisi
;;3) Troskovi
;;4) Dodaj trosak
;;5) Sledeci mali servis
;;0) Izlaz

;;izbor:0
;;Exit
;;
;;Process finished with exit code 0