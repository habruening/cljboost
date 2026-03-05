(ns cljboost)

;;;; A let macro that lets you escape in case something does not work like you want.

(defmacro let
  "This is drop-in replacement for `let`. Please see the documentation of `clojure.core/let`. Like let this
   macro accepts a list of bindings and a body.
   Additioanlly it accepts the keyword :escape followed by an expression in the list of bindings. This
   provides a simple mechanism to early escape the execution before more expressions are bound or the body
   is executed.
   
   Example:

   ````
   (let [a (read)
         b (read)
         :escape (when (zero? b) :divide-by-zero-prevented)]
     (/ a b))
   ````

   The formatting can often support the readability of the code.

   ````
   (let [a (read)
         b (read) :escape (when (zero? b) :divide-by-zero-prevented)]
     (/ a b))
   ````

   This let macro is often used with `:escape (when ...)`. But it works for all expressions that return
   falsy.

   ````
   (let [a (read)
         b (read)
         safety-check (if (zero? b) :divide-by-zero) 
         :escape safety-check]
     (/ a b))
   ````

   This let macro can be used in situation where something can go wrong, which should stop the execution.

   ````
   (require '[clojure.java.io :as io])

   (let [file-name \"test.dat\"
      :escape (when (not (.exists (io/file file-name))) :file-does-not-exist)
      file (slurp file-name)]
  (println file))
   ````
   "
  [bindings & body]
  (let [[b v & more-bindings] bindings]
    (cond (nil? b)
          `(do ~@body)
          (= b :escape)
          `(or ~v
               (let ~more-bindings ~@body))
          :else
          `(clojure.core/let [~b ~v]
             (let ~more-bindings ~@body)))))

;;;; A Clojure Exception mechanism

(defn fail [failure map]
  (if (not (contains? *expected-exceptions* failure))
    (throw (ex-info (str "Clojure Exceptions " (str failure) " not catched.") {})))
  (throw (ex-info "Clojure Exception" (assoc map :failure-dispatch-val failure))))

;;; Remember in the call tree what exceptions are catched.

(def ^:dynamic *expected-exceptions* #{})

(defmacro do-or-fail
  ""
  [expr & failure-recoveries]
  (let [recovery-clauses (partition 2 failure-recoveries)
        captured-failures (mapv first (map first recovery-clauses))
        ex-symbol `ex#
        recovery-case-clauses (reduce (fn [clauses [[failure binding-name] body]]
                                        (into clauses [failure `(let [~binding-name (ex-data ~ex-symbol)]
                                                                  ~body)]))
                                      []
                                      recovery-clauses)]
    `(binding [*expected-exceptions* (into *expected-exceptions* ~captured-failures)]
       (try ~expr
            (catch clojure.lang.ExceptionInfo ~ex-symbol
              (case ((ex-data ~ex-symbol) :failure-dispatch-val)
                ~@recovery-case-clauses
                (throw ~ex-symbol)))))))

(comment (do-or-fail (fail :uuu {})
                     [:error error] (println error)
                     [:uuu error] (println error))

         (macroexpand-1 '(do-or-fail 3 [[:ddd error] (psdf)
                                        [:uuu error] (asdf)]))

         (do-or-fail (open-file)
                     [:divide-by-zero error] (println "Error: " error)
                     [:not-a-number error] (println "Error"))
         )

(defmacro defn [name & args]
  (let [[doc-string & more-args] args
        [doc-string args] (if (string? doc-string)
                            [doc-string more-args]
                            [nil args])
        [attr-map & more-args] args
        [attr-map [params & args]] (if (map? attr-map)
                                     [attr-map more-args]
                                     [nil args])
        [prepost-map & more-args] args
        [prepost-map [& body]] (if (map? prepost-map)
                                 [prepost-map more-args]
                                 [nil args])
        fails-with (if prepost-map (prepost-map :fails-with))
        prepost-map (dissoc prepost-map :fails-with)
        attr-map (assoc attr-map :fails-with (if prepost-map (prepost-map :fails-with))
                                 :prevents (if prepost-map (prepost-map :prevents))
                                 :checks (if prepost-map (prepost-map :checks)))]
    (let [code (conj body
                     `(if (not (clojure.set/subset? ~fails-with *expected-exceptions*))
                        (throw (ex-info ~(str "Clojure Exceptions " (str fails-with) " not catched.") {}))))
          code (if prepost-map (conj code prepost-map) code)
          code (conj code params)
          code (if attr-map (conj code attr-map) code)
          code (if doc-string (conj code doc-string) code)
          code (conj code name)
          code (conj code 'clojure.core/defn)]
      code)))


(comment (macroexpand-1 '(defn a "a" {:a 3} [u] {:pre 3 :fails-with #{:a}} (println "asd")))
         (macroexpand-1 '(defn a [u v] (println "asd")))

         (defn load-file [file-name]
           {:fails-with #{:a}}
           (if (= file-name 3) (fail :b {})))

         (def x ^:a [3])

         (def m ^:hi [1 2 3])
         (meta m)

         (defn m "33" {:hi 7} [] {:dfs "sdf"} :ok)
         (meta #'m)
         
         (clojure.core/defn n {:a 3} [] nil)

         (meta n)
         (clojure.core/defn xyz {:a 3} [] nil)
         (meta xyz)

         (meta load-file)

         (do-or-fail (load-file 3)
                     [:a error] (str "Error:" error)
                     [:b error] (str "Error:" error))
         )

(meta-info load-file)


(defmacro check-and-do )
(check-and-do [#{:a :b} error (file-does-not-exist "test.txt")] (create-file)
              (load-file "test.txt"))

(ensure-and-do )

(load-file 4)


(defn open-file.exists?
  "Open a new file"
  [file-name]
  {:checks #{:file-not-found}}
  (slurp file-name))

(defn open-file.exists?
  "Open a new file"
  [file-name]
  {:prevents #{:file-not-found}}
  (slurp file-name))