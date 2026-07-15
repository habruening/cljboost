(ns test)

(defn test-and-expect [[expr arrow & args]]
  (cond (= arrow '=>) [`(clojure.test/is (= ~expr ~(first args)))
                       (next args)]
        (= arrow '=fn>) (test-and-expect (into [`(~(first args) ~expr)] (rest args)))
        :else [expr (cond args (conj (if args args '()) arrow)
                          arrow (list arrow))]))

(defmacro protocol [& args]
  (loop [lines []
         more-args args]
    (if (not more-args) lines
        (let [[test still-more-args] (test-and-expect more-args)]
          (recur (conj lines test)
                 still-more-args)))))
