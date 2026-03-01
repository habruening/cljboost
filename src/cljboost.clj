(ns cljboost)

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
