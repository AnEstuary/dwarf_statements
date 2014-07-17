(ns dwarf-statements.core
    "parse data from xml file"
  (:use [clojure.data.zip.xml :as c-d-z-xml
                :refer [xml-> xml1-> attr attr= text]]
        [clojure.data.xml :as c-d-xml :refer [parse]]
        [clojure.zip :as c-zip :refer [xml-zip]]))

(require '[clojure.walk :as w])



; This let parses the xml file, reads the name of all sites in our XMLfile and puts them in a vector
; The plan is to use the principles from this code to parse our data for xAPI



;(defn xml-dwarf-site-parser
; "This function parses information about sites from the given
; dwarven XML file path and stores it on a list"
;  [final-list file-path]
;  (let [xml-file (clojure.java.io/reader file-path)]
;    (doseq [x (xml-seq (parse xml-file :coalescing false))
;      :when (= :sites (:tag x))]
;        (doseq [y (:content x)
;          :when (= :site (:tag y))]
;          (doseq [nimi   (:content y)
;                  tag    (:content y)
;                  coords (:content y)
;             :when (= :name (:tag nimi))
;             :when (= :id    (:tag tag))
;             :when (= :coords (:tag coords))]
;            (conj! final-list {:name (first (:content nimi)), :id (first (:content tag)), :coords (first (:content coords))})
;            )))))



(defn sites->map [element]
  (let [z (xml-zip element)]
      {:id (xml1-> z :id text)
       :name (xml1-> z :name text)
       :coords (xml1-> z :coords text)}))


(with-open [rdr(clojure.java.io/reader "resources/data/region2-legends.xml")]
        (doall
          (take 35
             (->> rdr
                  parse
                  :content
                  (filter #(= :sites (:tag %)))
                  first
                  :content
             (map sites->map)))))



;(def parsed-site-data (transient []))
;(xml-dwarf-site-parser parsed-site-data "resources/data/region2-legends.xml")


;(for[i (range (count parsed-site-data))]
;    (parsed-site-data i))


;(let [xml-file (clojure.java.io/reader "resources/data/region2-legends.xml")]
;  (type(xml-seq (parse xml-file))))
