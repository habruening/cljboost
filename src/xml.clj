;; Important Design Decisions:
;;
;; 1. Is it possible to traverse up or back?
;;
;; It is a design decision, that queries only ever work forward. This enables laziness, so that
;; we can work with infininte xml data. If we need something later, we will need a mechanism
;; to capture data while processing the query.
;;
;; 2. Why query not look at the current node?
;;
;; The code would simplify by changing the mechanism in that way that it applies the query
;; starting at the current node instead of the child nodes. However, this is not possible.
;; Consider you want get the third child node. Wen starting the query at the current note, it
;; this is impossible, because the child nodes do not know at what position they are. We would
;; need a preprocessing.
;;
;; 3. Why not something more module (e.g. multimethods)
;; Tried out clojure.match
;; Tried out multipethods
;;    

(def query)
(def query*)

(comment
  (require '[clojure.data.xml :as dxml])

  (def example-xml
    (dxml/parse
     (java.io.StringReader. "<lecture name='Computer Science 1'>
                                       <docent>Alan Turing</docent>
                                       <audience> 
                                         <person userid='pt23'>
                                           Peter
                                           <subject>Math</subject>
                                         </person>
                                         <material>xy.pdf</material>
                                         <person userid='hk09'>
                                           Hank
                                           <subject>Pysics</subject>
                                         </person>
                                         <person guest='true'>Jon</person>
                                       </audience>
                                     </lecture>")))
  example-xml)

(defn value [tag]
  (:content tag))

(comment
  (value (dxml/parse (java.io.StringReader. "<person>Hank</person>")))
  (value (dxml/parse (java.io.StringReader. "<person></person>"))))

(defn single-query [tag basic-query]
  (cond

    (symbol? basic-query)
    (filter #(and (map? %)
                  (-> % :tag name (= (name basic-query)))) (:content tag))

    (keyword? basic-query)
    (if-let [value (basic-query (:attrs tag))]
      (list value)
      '())

    (integer? basic-query)
    (if (< basic-query (count (:content tag)))
      (list (nth (:content tag) basic-query)))

    (vector? basic-query)
    (if (not (empty? (apply query tag basic-query)))
      (list tag)) 

    (fn? basic-query)
    (basic-query tag)))

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
  (single-query example-xml #(count (:content %)))
  )
  
(defn query [tag-or-tags first-basic-query & more-basic-queries] 
  (cond
    
    (map? tag-or-tags)
    (let [first-results (single-query tag-or-tags first-basic-query)]
      (if (not more-basic-queries)
        first-results
        (apply query first-results more-basic-queries)))
    
    (seq? tag-or-tags)
    (let [query-in-sub-node
          (fn [result tag]
            (concat result (apply query tag first-basic-query more-basic-queries)))]
      (reduce query-in-sub-node '() tag-or-tags))))


(comment
  ;; Testing query with a single tag and a longer query
  (query example-xml 'audience 'person 'subject value)

  ;; Testing query with multiple tags
  (query (query example-xml 'audience 'person)
         'subject value)
  (query example-xml 'audience 1 value)
  (query (query example-xml 'audience 2)
         'subject value)
  (query (query example-xml 'audience #(take 1 (:content %)))
         'subject value)
  
  ;; Testing `single-query` with vectors
  (single-query (dxml/parse (java.io.StringReader. "<person><name/></person>"))
                ['date])
  (single-query (dxml/parse (java.io.StringReader. "<person><name/></person>"))
                ['name])
  (query example-xml 'audience 'person ['subject])

  ;; Hier weiter
  (query example-xml 'audience 'person :1)

  )
