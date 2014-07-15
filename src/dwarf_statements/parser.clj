(ns dwarf-statements.core
    "parse data from xml file"
  (:use [clojure.data.xml]))

(require '[clojure.walk :as w])


;(defn parse-xml
;  "parse xml file at path"
;  [file-path]
;  (let [input-xml (clojure.java.io/reader file-path)]
;  (parse input-xml)))

;(def xml-data (xml-seq (parse-xml "resources/data/region2-legends.xml")))


; This let parses the xml file, reads the name of all sites in our XMLfile and puts them in a vector
; The plan is to use the principles from this code to parse our data for xAPI

(let [xml-file (clojure.java.io/reader "resources/data/region2-legends.xml")
      a-list (transient [])]
  (doseq [x (xml-seq (parse xml-file))
    :when (= :sites (:tag x))]
      (doseq [y (:content x)
        :when (= :site (:tag y))]
          (doseq [nimi   (:content y)
                  tag    (:content y)
                  coords (:content y)
            :when (= :name   (:tag nimi))
            :when (= :id     (:tag tag))
            :when (= :coords (:tag coords))]
            (conj! a-list {:name (first (:content nimi)), :id (first (:content tag)), :coords (first (:content coords))}))))
  (for[i (range (count a-list))]
    (a-list i)))



(let [xml-file (clojure.java.io/reader "resources/data/region2-legends.xml")]
  (type(xml-seq (parse xml-file))))
