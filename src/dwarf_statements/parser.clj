(ns dwarf-statements.core
    "parse data from xml file"
  (:use [clojure.data.zip.xml :as c-d-z-xml
                :refer [xml-> xml1-> attr attr= text]]
        [clojure.data.xml :as c-d-xml :refer [parse]]
        [clojure.zip :as c-zip :refer [xml-zip]]))

(require '[clojure.walk :as w])

; This section is where we list our different map types for parsing. XML sections have dramatically different
; structures and properties, which require different maps. Some maps require the nested map grabber, which grab nested
; elements from different tags inside the map currently being looked at.

(defn skills->map [e]
  (let [z (xml-zip e)]
   {:skill    (xml1-> z :skill text)
    :total_ip (xml1-> z :total_ip text)}))

(defn links->map [e]
  (let [z (xml-zip e)]
    {:link_type (xml1-> z :link_type text)
     :entity_id (xml1-> z :entity_id text)}))

(defn sites->map [e]
  (let [z (xml-zip e)]
      {:id      (xml1-> z :id text)
       :name    (xml1-> z :name text)
       :coords  (xml1-> z :coords text)
       }))

(defn regions->map [e]
  (let [z (xml-zip e)]
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
     :birth_seconds   (xml1-> z :birth_seconds72 text)
     :death_year      (xml1-> z :death_year text)
     :death_seconds   (xml1-> z :death_seconds72 text)
     :associated_type (xml1-> z :associated_type text)
     :hf_skills       (nested-list-grabber e :hf_skill skills->map)
     :entity_links    (nested-list-grabber e :entity_link  links->map)
     }))




; this section is where we handle functions for lazy sequences.

(defn nested-list-grabber
  "This function grabs the nested list tag
   from element and maps them to according to fn coll."
  [element tag coll]
    (doall (->> element
                :content
                (filter #(= tag (:tag %)))
                (map coll))))


(defn parse-dwarf-xml
  "This function takes the address of the file
  you wish to parse, finds the tag of the items
  you wish to collect, and maps the items according to fn coll"
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

; This is where we call our parsing function and watch magic happen

(parse-dwarf-xml "resources/data/region2-legends.xml" :regions regions->map)
(parse-dwarf-xml "resources/data/region2-legends.xml" :sites sites->map )
(parse-dwarf-xml "resources/data/region2-legends.xml" :underground_regions underground-regions->map)
(parse-dwarf-xml "resources/data/region1-legends.xml" :historical_figures historical-figures->map )


