(ns test-test
  (:require [clojure.test :refer :all]
            [test :refer :all]))

(deftest protocol-tests
  (testing "protocol"
    (is (= (protocol (inc 4) => 5
                     (dec 4) => 3
                     (+ 2 3) => 5
                     (println "Hallo")
                     (+ 2 3) => 5)
           [true true true nil true]))
    (is (= (protocol
            3 =fn> inc => 4)))))
