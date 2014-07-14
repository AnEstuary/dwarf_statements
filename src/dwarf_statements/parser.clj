(ns dwarf-statements.core
    "parse data from xml file"
  (:use [clojure.data.xml]))


(defn parse-xml
  "parse xml file at path"
  [file-path]
  (let [input-xml (clojure.java.io/reader file-path)]
  (parse input-xml)))

(let [xml-file (clojure.java.io/reader "resources/data/region2-legends.xml")]
  (doseq [x (xml-seq (parse xml-file))
    :when (= :sites (:tag x))]
      (doseq [y (:content x)
        :when (= :site (:tag y))]
          (doseq [z (:content y)
            :when(= :name (:tag z))]
              (println (first (:content z)))))))


(parse-xml "resources/data/region2-legends.xml")

((parse-xml "resources/data/test.xml"))
