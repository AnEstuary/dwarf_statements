(ns dwarf-statements.core
    "parse data from xml file"
  (:use [clojure.data.zip.xml :as c-d-z-xml
                :refer [xml-> xml1-> attr attr= text]]
        [clojure.data.xml :as c-d-xml :refer [parse]]
        [clojure.zip :as c-zip :refer [xml-zip]]))

(require '[clojure.walk :as w])



; this section is where we handle functions for lazy sequences.

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

(defn nested-list-grabber
  "This function grabs the nested list tag
   from element and maps them to according to fn coll."
  [element tag coll]
    (doall (->> element
                :content
                (filter #(= tag (:tag %)))
                (map coll))))


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

(defn site-links->map [e]
  (let [z (xml-zip e)]
    {:link_type (xml1-> z :link_type text)
     :site_id (xml1-> z :site_id text)}))

(defn spheres->map [e]
  (let [z (xml-zip e)]
    {:sphere (xml1-> z text)}))



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
     :site_links      (nested-list-grabber e :site_link site-links->map)
     :spheres         (nested-list-grabber e :sphere spheres->map)
     :ent_pop_ids     (xml1-> z :ent_pop_id text)
     }))

(defn historical-event-type-cases
  "this event returns a different list
  of keys and values to look for depending
  on the event type in historical-events"
  [event-type z]
  (case event-type
    "change hf state"
      {:hfid              (xml1-> z :hfid text)
       :state             (xml1-> z :state text)
       :site_id           (xml1-> z :site_id text)
       :subregion_id      (xml1-> z :subregion_id text)
       :feature_layer_id  (xml1-> z :feature_layer_id text)
       :coords            (xml1-> z :coords text)}
    "add hf entity link"
      {:civ_id            (xml1-> z :civ_id text)}
    "created site"
      {:civ_id            (xml1-> z :civ_id text)
       :site_civ_id       (xml1-> z :site_civ_id text)
       :site_id           (xml1-> z :site_id text)}
    "add hf hf link"
      {:hfid              (xml1-> z :hfid text)
       :hfid_target       (xml1-> z :hfid_target text)}
    "created structure"
      {:civ_id            (xml1-> z :civ_id text)
       :site_civ_id       (xml1-> z :site_civ_id text)
       :site_id           (xml1-> z :site_id text)
       :structure_id      (xml1-> z :structure_id text)}
    "change hf job"
      {:hfid              (xml1-> z :hfid text)
       :site_id           (xml1-> z :site_id text)
       :subregion_id      (xml1-> z :subregion_id text)
       :feature_layer_id  (xml1-> z :feature_layer_id text)}
    "hf travel"
      {:group_hfid        (xml1-> z :group_hfid text)
       :site_id           (xml1-> z :site_id text)
       :subregion_id      (xml1-> z :subregion_id text)
       :feature_layer_id  (xml1-> z :feature_layer_id text)}
    "hf abducted"
      {:target_hfid       (xml1-> z :target_hfid text)
       :snatcher_hfid     (xml1-> z :snatcher_hfid text)
       :site_id           (xml1-> z :site_id text)
       :subregion_id      (xml1-> z :subregion_id text)
       :feature_layer_id  (xml1-> z :feature_layer_id text)}
    "hf simple battle event"
      {:subtype           (xml1-> z :subtype text)
       :group_1_hfid      (xml1-> z :group_1_hfid text)
       :group_2_hfid      (xml1-> z :group_2_hfid text)
       :site_id           (xml1-> z :site_id text)
       :subregion_id      (xml1-> z :subregion_id text)
       :feature_layer_id  (xml1-> z :feature_layer_id text)}
    "hf died"
      {:hfid              (xml1-> z :hfid text)
       :slayer_hfid       (xml1-> z :slayer_hfid text)
       :slayer_race       (xml1-> z :slayer_race text)
       :slayer_caste      (xml1-> z :slayer_caste text)
       :slayer_item_id    (xml1-> z :slayer_item_id text)
       :slayer_shooter_item_id (xml1-> z :slayer_shooter_item_id text)
       :site_id           (xml1-> z :site_id text)
       :subregion_id      (xml1-> z :subregion_id text)
       :feature_layer_id  (xml1-> z :feature_layer_id text)
       :cause             (xml1-> z :cause text)}
    "changed creature type"
    {}
    "item stolen"            {}
    "creature devoured"      {}
    "hf gains secret goal"   {}
    "field battle"           {}
    "hf wounded"             {}
    "assume identity"        {}
    "remove hf entity link"  {}
    "hf new pet"             {}
    "artifact created"       {}
    "artifact stored"         {}
    "attacked site"           {}
    "plundered site"          {}
    "hf reunion"              {}
    "change hf body state"    {}
    "entity created"          {}
    "add hf site link"        {}
    "entity relocate"         {}
    "entity primary criminals"{}
    "hf profaned structure"   {}
    "hf does interaction"     {}
    "remove hf site link"     {}
    "created world construction" {}
    "artifact possessed"      {}
    "hf learns secret"        {}
    "peace accepted"          {}
    "razed structure"         {}
    "body abused"             {}
    "hf confronted"           {}
    "destroyed site"          {}
    "entity law"              {}
    "peace rejected"          {}
    "reclaim site"            {}
    "new site leader"         {}
    {}))

(defn historical-events->map [e]
  (let [z (xml-zip e)]
    (conj {:id (xml1-> z :id text)
     :year     (xml1-> z :year text)
     :seconds  (xml1-> z :seconds72 text)
     :type     (xml1-> z :type text)
     } (historical-event-type-cases (xml1-> z :type text) z))
    ))



; This is where we call our parsing function and watch magic happen

(parse-dwarf-xml "resources/data/region2-legends.xml" :regions regions->map)
(parse-dwarf-xml "resources/data/region2-legends.xml" :sites sites->map )
(parse-dwarf-xml "resources/data/region2-legends.xml" :underground_regions underground-regions->map)
(parse-dwarf-xml "resources/data/region2-legends.xml" :historical_figures historical-figures->map )
(parse-dwarf-xml "resources/data/region1-legends.xml" :historical_events historical-events->map )


