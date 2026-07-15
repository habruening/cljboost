(ns xml-test
  (:require [clojure.test :refer :all]
            [test :refer :all]
            [xml :refer :all]
            [clojure.data.xml :as dxml]))


(deftest test-value 
  (protocol
   (value (dxml/parse (java.io.StringReader. "<person>Hank</person>")))
   =fn> first => "Hank"
   (value (dxml/parse (java.io.StringReader. "<person></person>")))
   => '()))

(deftest test-single-query
  (let [example (dxml/parse (java.io.FileInputStream. "test/xml_test.xml"))]
    (testing "everything but, but vectors"
      (protocol

       (single-query example 'owner)
       =fn> first =fn> value =fn> first
       => "Green Company"

       (single-query example 'admin)
       =fn> #(map value %) =fn> #(apply concat %)
       => ["breg101" "ugof103"]

       (single-query example 'address) => '()

       (single-query example :host)
       =fn> first
       => "sxw32"

       (single-query example :address)
       => '()

       (single-query example 0)
       =fn> first =fn> value =fn> first
       => "Green Company"

       (single-query example 2)
       =fn> first =fn> value =fn> first
       => "ugof103"

       (single-query example 4)
       =fn> first =fn> clojure.string/trim
       => "more information"

       (single-query example 12)
       => '()

       (single-query example value) ; `value` queries all child nodes
       =fn> count
       => 6)
      (testing "with a vector"
        (protocol
         
         (query example 'admin [:active] value)
         => '("ugof103")
         
         (query example 'software 'tool ['license #(= '("proprietary") (:content %))] 'name value)
         => '("yEd" "Word" "Matlab")
         
         (query (sort-by #(first (query % 'name value))
                         (query example 'software 'tool ['license #(= '("proprietary") (:content %))]))
                'name value)
         => '("Matlab" "Word" "yEd"))))))

(let [example (dxml/parse (java.io.FileInputStream. "test/xml_test.xml"))]
  (query example 'software 'tool ['license #(= '("proprietary") (:content %))] stop
         identity #_(fn [x] #(first (query % 'name value)) (:content x))))


(comment
  ;; Testing `single-query` with symbols
  (single-query example-xml 'university)
  (-> (single-query example-xml 'docent)
      first value)

  (-> (dxml/parse (java.io.StringReader. "<entry>asdf</entry>"))
      (single-query 'asdf))

  ;; Testing `single-query` with attributes
  (single-query example-xml :room)
  (single-query example-xml :name)

  ;; Testing `single-query` with vectors later

  ;; Testing `single-query` with functions
  (single-query example-xml #(count (:content %))))
