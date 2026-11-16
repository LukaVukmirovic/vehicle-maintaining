(ns vehicle-maintaining.average-cost
  (:require [clojure.xml :as xml]))

(comment
  (def expences (xml/parse "xml-data/xpenses.xml"))

  (def podaci (map #(get-in % [1 :content 0]) (map :content (get-in expences [:content]))))

  (defn prosek [x y] (/ (+ x y) 2))

  (reduce prosek 0 podaci)

  (reduce prosek [0 (map #(get-in % [1 :content 0]) (map :content (get-in expences [:content])))])
  )