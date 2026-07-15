(ns core-test
  (:require [clojure.test :refer :all]
            [test :refer :all]
            [core :refer :all]))


(deftest elet-tests
  (testing "the normal `let` behaviour with the examples from the documentation."
    (protocol (elet [x 1] x)
              => (let [x 1] x))

    (do (elet [y 1]
              (def z y))
        (is (= 1 z)))
    (is (= (elet [a 1 b 2]
                 (+ a b))
           (let [a 1 b 2]
             (+ a b))))
    (is (= (elet [      ;;Binding Map
                  {:keys [k1 k2]        ;; bind vals with keyword keys
                   :strs [s1 s2]        ;; bind vals with string keys
                   :syms [sym1 sym2]    ;; bind vals with symbol keys
                   :or {k2 :default-kw, ;; default values
                        s2 :default-s,
                        sym2 :default-sym}
                   :as m}  ;; bind the entire map to `m`
                  ;;Data
                  {:k1 :keyword1, :k2 :keyword2,  ;; keyword keys
                   "s1" :string1, "s2" :string2,  ;; string keys
                   'sym1 :symbol1,                ;; symbol keys
                   ;; 'sym2 :symbol2              ;; `sym2` will get default value
                   }]
                 [k1 k2 s1 s2 sym1 sym2 m])
           (let [      ;;Binding Map
                 {:keys [k1 k2]        ;; bind vals with keyword keys
                  :strs [s1 s2]        ;; bind vals with string keys
                  :syms [sym1 sym2]    ;; bind vals with symbol keys
                  :or {k2 :default-kw, ;; default values
                       s2 :default-s,
                       sym2 :default-sym}
                  :as m}  ;; bind the entire map to `m`
                 ;;Data
                 {:k1 :keyword1, :k2 :keyword2,  ;; keyword keys
                  "s1" :string1, "s2" :string2,  ;; string keys
                  'sym1 :symbol1,                ;; symbol keys
                  ;; 'sym2 :symbol2              ;; `sym2` will get default value
                  }]
             [k1 k2 s1 s2 sym1 sym2 m]))))

  (testing "the escape behaviour"
    (is (= (elet [x 17
                  :escape false]
                 x)
           17))
    (is (= (elet [x 17
                  :escape true]
                 x)
           true))
    (is (= (elet [x 17
                  :escape "error"]
                 x)
           "error"))
    (is (= (elet [x 17
                  :escape "error"
                  file (slurp "does not exist")]
                 x)
           "error"))
    (is (= (elet [x 17
                  :escape (if (= x 16) "error")]
                 x)
           17))
    (is (= (elet [x 17
                  :escape (if (= x 17) "error")]
                 x)
           "error"))
    (is (= (elet [x 17
                  y (+ x 1)
                  :escape (if (= y 18) "error")]
                 x)
           "error"))))

(deftest str|-test 
  (is (= (str| "") "")))

(deftest str|-test
  (protocol
   (str| "") => ""
   (str| "   a") => "   a"
   (str| "a
         |b") => "a\nb"
   (str| "a
         |b
          c") => "a\nb\nc"
   (str| "a
         |b
     |___ c") => "a\nb\nc"
   
   (str| "a
         |b") => "a\nb"
   (str| "a
         |b
          ") => "a\nb\n"))
