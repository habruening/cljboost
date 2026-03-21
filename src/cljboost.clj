(ns cljboost)

;;;; A let macro that lets you escape in case something does not work like you want.

(defmacro let
  "This is drop-in replacement for `let`. Please see the documentation of `clojure.core/let`. Like let this
   macro accepts a list of bindings and a body.
   Additioanlly it accepts the keyword :escape followed by an expression in the list of bindings. This
   provides a simple mechanism to early escape the execution before more expressions are bound or the body
   is executed.
   
   Examples:

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
  (clojure.core/let [[b v & more-bindings] bindings]
    (cond (nil? b)
          `(do ~@body)
          (= b :escape)
          `(or ~v
               (let ~more-bindings ~@body))
          :else
          `(clojure.core/let [~b ~v]
             (let ~more-bindings ~@body)))))

;;;; A Clojure Failure mechanism

; We introduce the concept of Failures here. Technicaly a Failure is an exception of the type
; ExceptionInfo that carries in their data a key :failure-dispatch-val. A failure is all sorts of
; things that can go wrong or that deviate from the normal case or from the normal control flow.
; We can react on these failures by looking at the value of :failure-dispatch-val, that all
; failures carry.

; We will not call this mechanism "try" and "catch". Failures are a mechanism, which we all know from
; Exceptions. They are an alternative control flow, that is taken in case of something does not go
; the standard execution. But unlike exceptions failures are more controlled.
; While exceptions are normally a mechanism to react on unexpected situations, failures are also meant
; to be used for expected failures. For example if we open a file, it is not unexpected that we don't
; have read permissions. Therefore the word "exception" is not used. We use the word "Failure".

; Although Failures technically are exception, the following provides a mechanism for Failures that
; does not expose their nature. Failures are an alternative way of error handling and not meant to be
; used in conjunction with Exceptions.

; The following features are provided by the Failure mechanism.
;
; We can fail with the `fail` function. This immediatelly stops the current execution in a
; non-recoverable way. The `fail` must only ever be called in a context, where this sort of
; Failure is handled. This is called recovering. Just failing somehow is not allowed. We are
; only allowed to fail in ways that are expected.
;
; We can recover on Failures by enclosing potentially failing code into `do-and-recover`
; together with a recovery for all Failures that potentially could by occur.
;
; We can check Failures upfront before calling potentially failing code by enclosing it into
; `prevent-failure-and-do` and by providing code that upfront checks for Failures. In this
; case, the potentially failing code will not fail and does not require recoveries.
;
; We can check Failures upfront before calling potentially failing code by enclosing it into
; `prevent-failure-and-do` and by providing code that upfront prevents Failures. In this
; case, the potentially failing code will not fail and does not require recoveries.
;
; In both cases, is still not guaranteed that Failures cannot occur, because of things that
; are not in control of our code. For example if we create a file in order to prevent a file
; not found Failure, it can still happen that the file gets deleted by something, which we don't
; have under control. This sort of problems cannot be prevented. This is why the naming `-do-`
; is chosen. The developer is responsible for everything, which is not in control of the
; Clojure code.

;;; Remember which Failures have a reaction in the call tree up.
(def ^:dynamic *expected-failures* #{})


(def ^:dynamic *as-checker* nil)

(defn fail
  "Fail with `failure` and with a map of additional data. The Failure is typically a keyword, which
   represents an error code, but can also be anything else. This function must only be called in a
   context, where that Failure is checked, prevented or which has a recovery for that Failure.
   If the function is called in a context, where the failure is checked or prevented upfront,
   no recoveries on Failures are required. But if it is then called (although not expected), it
   will still fail. This can happen if the situation since the check changed or the prevention
   (for some reason) did not work or in case of programming errors.
   
   Examples"
  [failure map]
  (if (not (contains? *expected-failures* failure))
    (throw (ex-info (str "Clojure Exception " (str failure) " has no recovery.") {})))
  (throw (ex-info "Clojure Exception" (assoc map :failure-dispatch-val failure))))

(defmacro do-and-recover
  "Execute and return the expresssion unless it fails. In case the evaluation fails, an appropriate
   reaction is executed. The failure-recoveries are used to decide how to react and to evaluate and
   return that reaction. Each failure recovery is a vector of two elements. The first entry is the
   failure dispatch and binding-form of the failure. The second is the body of the recovery while
   the symbols of the binding-form are bound.
   
   Examples:
   ````
   (do-and-recover (let [input (read)]
     (cond 
       (not (number? input)) (fail :not-a-number {:input input})
       (< -0.0001 input 0.0001) (fail :divide-by-zero {})
       input {:result (/ 12.0 input)}))
     [:not-a-number {input :input}] {:result nil :input input :error :not-a-number}
     [:divide-by-zero error] {:result nil :error :divide-by-zero})
   ````
   A complete example
   ````
   (defn load-config []
     (try (slurp \"example.cfg\")
          (catch java.io.FileNotFoundException _
            (fail :no-conf-file {:filename \"example.cfg\"}))))
   
   (do-and-recover (load-config)
                   [:no-conf-file {filename :filename}] (do (spit filename \"#auto-created\") \"\"))
   ````
   
   "
  [expr & failure-recoveries]
  (let [recovery-clauses (partition 2 failure-recoveries)
        captured-failures (mapv first (map first recovery-clauses))
        ex-symbol `ex#
        recovery-case-clauses (reduce (fn [clauses [[failure binding-name] body]]
                                        (into clauses [failure `(let [~binding-name (ex-data ~ex-symbol)]
                                                                  ~body)]))
                                      []
                                      recovery-clauses)]
    `(binding [*expected-failures* (into *expected-failures* ~captured-failures)]
       (try ~expr
            (catch clojure.lang.ExceptionInfo ~ex-symbol
              (case ((ex-data ~ex-symbol) :failure-dispatch-val)
                ~@recovery-case-clauses
                (throw ~ex-symbol)))))))

(comment (do-and-recover (let [input (read)]
                           (cond (not (number? input)) (fail :not-a-number {:input input})
                                 (< -0.0001 input 0.0001) (fail :divide-by-zero {})
                                 input {:result (/ 12.0 input)}))
                         [:not-a-number {input :input}] {:result nil :input input :error :not-a-number}
                         [:divide-by-zero error] {:result nil :error :divide-by-zero})

         (defn load-config []
           (try (slurp "example.cfg")
                (catch java.io.FileNotFoundException _
                  (fail :no-conf-file {:filename "example.cfg"}))))

         (do-and-recover (load-config)
                         [:no-conf-file {filename :filename}] (do (spit filename "#auto-created") ""))

         (macroexpand-1 '(do-and-recover (body)
                                         [:failure-1 error] (recovery-failure-1 error)
                                         [:failure-2 error] (recovery-failure-2 error)))
         )



(defmacro bdefn [name & args]
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
        ; The failures, that are raised by this function. We have to ensure that these are catched by callers. If not, we immediatelly fail the call.
        fails-with (:fails-with prepost-map)
        ; Those failures that are checked of prevented by this function.
        check-map (select-keys prepost-map [:fails-with :prevents :checks])
        prepost-map (dissoc prepost-map :fails-with :prevents :checks)]
    (let [; It the function is a checker (which is known at compile time) and is executed as a checker
          ; (which is known at runtime), we have to tell the caller, what the function is checking together
          ; with the result (which is the body). 
          code (if (not-empty check-map)
                 `(let [result# (do ~@body)]
                    (if *as-checker*
                      ~(assoc check-map :result `(binding [*as-checker* nil]
                                                   result#))
                      result#))
                 body)
          ; If the function raises failures, we have to ensure, that all failures are catched by the caller.
          code (if (not-empty fails-with)
                  (conj body
                        `(if (not (clojure.set/subset? ~fails-with *expected-failures*))
                           (throw (ex-info ~(str "Clojure Exceptions " (str fails-with) " not catched.") {}))))
                  code)
          code (if (not-empty prepost-map) (conj code prepost-map) code)
          code (conj code params)
          code (if attr-map (conj code attr-map) code)
          code (if doc-string (conj code doc-string) code)
          code (conj code name)
          code (conj code 'clojure.core/defn)
          ]
      code)))

(macroexpand-1 '(bdefn a "dd" {:doc "sf"} [u] {:prevents 3 :checks 4 :pre :a} 4 5))

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

         (do-and-recover (load-file 3)
                     [:a error] (str "Error:" error)
                     [:b error] (str "Error:" error))
         )

(prevent-failures-and-do)

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