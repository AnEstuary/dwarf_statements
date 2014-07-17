(ns dwarf-statements.core
    "parse data from xml file"
  (:use [clojure.data.zip.xml :as c-d-z-xml
                :refer [xml-> xml1-> attr attr= text]]
        [clojure.data.xml :as c-d-xml :refer [parse]]
        [clojure.zip :as c-zip :refer [xml-zip]]))

(require '[clojure.walk :as w])


(defn sites->map [e]
  (let [z (xml-zip e)]
      {:id      (xml1-> z :id text)
       :name    (xml1-> z :name text)
       :coords  (xml1-> z :coords text)
       }))

(defn regions->map [x]
  (let [z (xml-zip x)]
    {:id   (xml1-> z :id text)
     :name (xml1-> z :name text)
     :type (xml1-> z :type text)
     }))

(defn underground-regions->map [e]
  (let [z (xml-zip e)]
    {:id    (xml1-> z :id text)
     :type  (xml1-> z :type text)
     :depth (xml1-> z :depth text)}))


(defn historical-figures->map [e]
  (let [z (xml-zip e)]
    {:id              (xml1-> z :id text)
     :race            (xml1-> z :race text)
     :name            (xml1-> z :name text)
     :caste           (xml1-> z :caste text)
     :appeared        (xml1-> z :appeared text)
     :birth_year      (xml1-> z :birth_year text)
     :birth_seconds   (xml1-> z :birth_seconds text)
     :death_year      (xml1-> z :death_year text)
     :death_seconds   (xml1-> z :death_seconds text)
     :associated_type (xml1-> z :associated_type text)
     }))



;(with-open [rdr(clojure.java.io/reader "resources/data/region1-legends.xml")]
;  (doall
;    (->> rdr
;      parse
;      :content
;      (filter #(= :sites (:tag %)))
;      first
;      :content
;      (map sites->map))))

(defn parse-dwarf-xml
  "This function takes the address of the file
  you wish to parse, finds the tag of the items
  you wish to collect, and maps the items to collection coll"
  [address tag coll]
  (with-open [rdr(clojure.java.io/reader address)]
    (doall
      (->> rdr
           parse
           :content
           (filter #(= tag (:tag %)))
           first
           :content
           (map coll)))))

(parse-dwarf-xml "resources/data/region1-legends.xml" :regions regions->map)
(parse-dwarf-xml "resources/data/region1-legends.xml" :sites sites->map )
(parse-dwarf-xml "resources/data/region1-legends.xml" :underground_regions underground-regions->map)



;(def parsed-site-data (transient []))
;(xml-dwarf-site-parser parsed-site-data "resources/data/region2-legends.xml")


;(for[i (range (count parsed-site-data))]
;    (parsed-site-data i))


;(let [xml-file (clojure.java.io/reader "resources/data/region2-legends.xml")]
;  (type(xml-seq (parse xml-file))))
