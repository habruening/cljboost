(ns core)

;;;; A let macro that lets you escape in case something does not work like you want.

(defmacro elet
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
               (elet ~more-bindings ~@body))
          :else
          `(clojure.core/let [~b ~v]
             (elet ~more-bindings ~@body)))))

(defmacro str| [s]
  (elet [lines (clojure.string/split-lines s)
         :escape (when (< (count lines) 2) s)
         indentation (clojure.string/index-of (second lines) "|")
         :escape (when (not indentation) s)
         unindented-lines (map #(subs % (inc indentation)) (rest lines))]
        (str (first lines) "\n"
             (clojure.string/join "\n" unindented-lines))))

