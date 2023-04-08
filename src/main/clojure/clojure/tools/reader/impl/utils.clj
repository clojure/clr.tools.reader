;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.tools.reader.impl.utils
  (:refer-clojure :exclude [char reader-conditional tagged-literal]))

(defn char [x]
  (when x
    (clojure.core/char x)))

(def <=clojure-1-7-alpha5
  (let [{:keys [minor qualifier]} *clojure-version*]
    (or (< minor 7)
        (and (= minor 7)
             (= "alpha"
                (when qualifier
                  (subs qualifier 0 (dec (count qualifier)))))
             (<= (read-string (subs qualifier (dec (count qualifier))))
                5)))))

#_(defmacro compile-when [cond & then]             -- compile-when has been added to ClojureCLR itself
  (when (eval cond)
    `(do ~@then)))

(defn ex-info? [ex]
  (instance? clojure.lang.ExceptionInfo ex))

(compile-when <=clojure-1-7-alpha5
  (defrecord TaggedLiteral [tag form])

  (defn tagged-literal?
    "Return true if the value is the data representation of a tagged literal"
    [value]
    (instance? clojure.tools.reader.impl.utils.TaggedLiteral value))

  (defn tagged-literal
    "Construct a data representation of a tagged literal from a
       tag symbol and a form."
    [tag form]
    (clojure.tools.reader.impl.utils.TaggedLiteral. tag form))

  (ns-unmap *ns* '->TaggedLiteral)
  (ns-unmap *ns* 'map->TaggedLiteral)

  (defmethod print-method clojure.tools.reader.impl.utils.TaggedLiteral [o ^System.IO.TextWriter w]  ;;; ^java.io.Writer
    (.Write w "#")                                                                                   ;;; .write
    (print-method (:tag o) w)
    (.Write w " ")                                                                                   ;;; .write
    (print-method (:form o) w))

  (defrecord ReaderConditional [splicing? form])
  (ns-unmap *ns* '->ReaderConditional)
  (ns-unmap *ns* 'map->ReaderConditional)

  (defn reader-conditional?
    "Return true if the value is the data representation of a reader conditional"
    [value]
    (instance? clojure.tools.reader.impl.utils.ReaderConditional value))

  (defn reader-conditional
    "Construct a data representation of a reader conditional.
       If true, splicing? indicates read-cond-splicing."
    [form splicing?]
    (clojure.tools.reader.impl.utils.ReaderConditional. splicing? form))

  (defmethod print-method clojure.tools.reader.impl.utils.ReaderConditional [o ^java.io.Writer w]
    (.write w "#?")
    (when (:splicing? o) (.write w "@"))
    (print-method (:form o) w)))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (Char/IsWhiteSpace ^Char ch)                    ;;; Character/isWhitespace ^Character
        (identical? \,  ch))))

(defn numeric?
  "Checks whether a given character is numeric"
  [ch]                                            ;;; ^Character -- can't replace w/ Char becvuase Char is primitis
  (when ch
    (Char/IsDigit ^Char ch)))                                 ;;; Character/isDigit,  added ^Char

(defn newline?
  "Checks whether the character is a newline"
  [c]
  (or (identical? \newline c)
      (nil? c)))

(defn desugar-meta
  "Resolves syntactical sugar in metadata" ;; could be combined with some other desugar?
  [f]
  (cond
    (keyword? f) {f true}
    (symbol? f)  {:tag f}
    (string? f)  {:tag f}
    :else        f))

(defn make-var
  "Returns an anonymous unbound Var"
  []
  (with-local-vars [x nil] x))

(defn namespace-keys [ns keys]
  (for [key keys]
    (if (or (symbol? key)
            (keyword? key))
      (let [[key-ns key-name] ((juxt namespace name) key)
            ->key (if (symbol? key) symbol keyword)]
        (cond
          (nil? key-ns)
          (->key ns key-name)

          (= "_" key-ns)
          (->key key-name)

          :else
          key))
      key)))

(defn second' [[a b]]
  (when-not a b))

  
;;; DM: ADDED FOR COMPATIBILITY WITH ClojureCLR 1.8.
;;; These will not be needed in ClojureCLR 1.9 and later.

(defn char-value-in-radix 
  "Reproduces clojure.lang.LispReader.CharValueInRadix.  It was private in ClojureCLR 1.8 and earlier.
   This handles most of the cases (except wide Latin digits) of the Java Character/digit"
   [c radix]
   (cond
     (Char/IsDigit (char c)) 
	   (let [x (- c (long \0))] (if (< x radix) x -1))
	 (<= (long \A) c (long \Z)) 
	   (let [x (- c (long \A))] (if (< x (- radix 10)) (+ x 10) -1))
	 (<= (long \a) c (long \z)) 
	   (let [x (- c (long \a))] (if (< x (- radix 10)) (+ x 10) -1))
	 :else 
	   -1))
	 
(def ^:private ^System.Reflection.FieldInfo JReMatcher-match-fieldinfo 
  (.GetField clojure.lang.JReMatcher "_match" (enum-or System.Reflection.BindingFlags/NonPublic System.Reflection.BindingFlags/Instance)))
	 
(defn normalized-re-group 
  "JReMatcher in Clojure 1.8 and before is not identical to java.util.regex.Matcher on the group method.
   The Java version returns null on an unsuccessful subexpression match.  JReMatcher returns the empty string.
   Call (normalized-re-group matcher i) instead of (.group matcher i)  when this matters."
   [^clojure.lang.JReMatcher m index]
   (let [ match ^System.Text.RegularExpressions.Match (.GetValue JReMatcher-match-fieldinfo m)
         groups ^System.Text.RegularExpressions.GroupCollection (.Groups match)
		 group ^System.Text.RegularExpressions.Group (.get_Item groups (int index))]
	  (if (.Success group)
	    (.Value group)
		nil)))   
   
(defn add-front
  "Add sequence of items to the front of a System.Collections.Generic.LinkedList.
   This is supposed to handle the same situation as (.addAll ^java.util.List lst 0 items) used in the conditonal form splicer code.
   Someday, find a better way to code this."
   [^|System.Collections.Generic.LinkedList`1[System.Object]| lst items]
   (let [ first-node (.First lst)]
	  (doseq [item (reverse items)]
        (if first-node 
		  (.AddAfter lst first-node item)
		  (.AddFirst lst item)))))