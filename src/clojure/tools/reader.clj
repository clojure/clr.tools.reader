;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "A clojure reader in clojure"
      :author "Bronsa"}
  clojure.tools.reader
  (:refer-clojure :exclude [read read-line read-string char
                            default-data-readers *default-data-reader-fn*
                            *read-eval* *data-readers* *suppress-read*])
  (:require [clojure.tools.reader.reader-types :refer
             [read-char unread peek-char indexing-reader?
              get-line-number get-column-number get-file-name string-push-back-reader log-source]]
            [clojure.tools.reader.impl.utils :refer :all] ;; [char ex-info? whitespace? numeric? desugar-meta]
            [clojure.tools.reader.impl.errors :as err]
            [clojure.tools.reader.impl.commons :refer :all]
            [clojure.tools.reader.default-data-readers :as data-readers])
  (:import (clojure.lang PersistentHashSet IMeta
                         RT Symbol Reflector Var IObj
                         PersistentVector IRecord Namespace)
                                                                                     ;;; java.lang.reflect.Constructor
           (System.Text.RegularExpressions Regex)))                                   ;;; (java.util regex.Pattern List LinkedList)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare ^:private read*
         macros dispatch-macros
         ^:dynamic *read-eval*
         ^:dynamic *data-readers*
         ^:dynamic *default-data-reader-fn*
         ^:dynamic *suppress-read*
         default-data-readers)

(defn ^:private ns-name* [x]
  (if (instance? Namespace x)
    (name (ns-name x))
    (name x)))

(defn- macro-terminating? [ch]
  (case ch
    (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\) true
    false))

(defn- ^String read-token
  "Read in a single logical token from the reader"
  [rdr kind initch]
  (if-not initch
    (err/throw-eof-at-start rdr kind)
    (loop [sb (StringBuilder.) ch initch]
      (if (or (whitespace? ch)
              (macro-terminating? ch)
              (nil? ch))
        (do (when ch
              (unread rdr ch))
            (str sb))
        (recur (.Append sb ch) (read-char rdr))))))                         ;;; .append

(declare read-tagged)

(defn- read-dispatch
  [rdr _ opts pending-forms]
  (if-let [ch (read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch opts pending-forms)
      (read-tagged (doto rdr (unread ch)) ch opts pending-forms)) ;; ctor reader is implemented as a tagged literal
    (err/throw-eof-at-dispatch rdr)))

(defn- read-unmatched-delimiter
  [rdr ch opts pending-forms]
  (err/throw-unmatch-delimiter rdr ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-regex
  [rdr ch opts pending-forms]
  (let [sb (StringBuilder.)]
    (loop [ch (read-char rdr)]
      (if (identical? \" ch)
        (Regex. (str sb))                                                                 ;;; Pattern/compile
        (if (nil? ch)
          (err/throw-eof-reading rdr :regex sb)
          (do
            (.Append sb ch )                                                               ;;; .append
            (when (identical? \\ ch)
              (let [ch (read-char rdr)]
                (if (nil? ch)
                  (err/throw-eof-reading rdr :regex sb))
                (.Append sb ch)))                                                          ;;; .append
            (recur (read-char rdr))))))))

(defn- read-unicode-char
  ([^String token ^long offset ^long length ^long base]
   (let [l (+ offset length)]
     (when-not (== (count token) l)
       (err/throw-invalid-unicode-literal nil token))
     (loop [i offset uc 0]
       (if (== i l)
         (char uc)
         (let [d (char-value-in-radix (int (nth token i)) (int base))]                            ;;; Character/digit
           (if (== d -1)
             (err/throw-invalid-unicode-digit-in-token nil (nth token i) token)
             (recur (inc i) (long (+ d (* uc base))))))))))

  ([rdr initch base length exact?]
   (let [base (long base)
         length (long length)]
     (loop [i 1 uc (long (char-value-in-radix (int initch) (int base)))]                          ;;; Character/digit
       (if (== uc -1)
         (err/throw-invalid-unicode-digit rdr initch)
         (if-not (== i length)
           (let [ch (peek-char rdr)]
             (if (or (whitespace? ch)
                     (macros ch)
                     (nil? ch))
               (if exact?
                 (err/throw-invalid-unicode-len rdr i length)
                 (char uc))
               (let [d (char-value-in-radix (int ch) (int base))]                                 ;;; Character/digit
                 (read-char rdr)
                 (if (== d -1)
                   (err/throw-invalid-unicode-digit rdr ch)
                   (recur (inc i) (long (+ d (* uc base))))))))
           (char uc)))))))

(def ^:private ^:const upper-limit (int \uD7ff))
(def ^:private ^:const lower-limit (int \uE000))

(defn- read-char*
  "Read in a character literal"
  [rdr backslash opts pending-forms]
  (let [ch (read-char rdr)]
    (if-not (nil? ch)
      (let [token (if (or (macro-terminating? ch)
                          (whitespace? ch))
                    (str ch)
                    (read-token rdr :character ch))
            token-len (count token)]
        (cond

         (== 1 token-len)  (char (nth token 0))                                                    ;;; Character/valueOf 

         (= token "newline") \newline
         (= token "space") \space
         (= token "tab") \tab
         (= token "backspace") \backspace
         (= token "formfeed") \formfeed
         (= token "return") \return

         (.StartsWith token "u")                                                                   ;;; .startsWith
         (let [c (read-unicode-char token 1 4 16)
               ic (int c)]
           (if (and (> ic upper-limit)
                    (< ic lower-limit))
             (err/throw-invalid-character-literal rdr (.ToString ic "x"))			               ;;; (Integer/toString ic 16)		
             c))

         (.StartsWith token "o")                                                                    ;;; .startsWith
         (let [len (dec token-len)]
           (if (> len 3)
             (err/throw-invalid-octal-len rdr token)
             (let [uc (read-unicode-char token 1 len 8)]
               (if (> (int uc) 0377)
                 (err/throw-bad-octal-number rdr)
                 uc))))

         :else (err/throw-unsupported-character rdr token)))
      (err/throw-eof-in-character rdr))))

(defn ^:private starting-line-col-info [rdr]
  (when (indexing-reader? rdr)
    [(get-line-number rdr) (int (dec (int (get-column-number rdr))))]))

(defn ^:private ending-line-col-info [rdr]
  (when (indexing-reader? rdr)
    [(get-line-number rdr) (get-column-number rdr)]))

(defonce ^:private READ_EOF (Object.))
(defonce ^:private READ_FINISHED (Object.))

(def ^:dynamic *read-delim* false)
(defn- ^PersistentVector read-delimited
  "Reads and returns a collection ended with delim"
  [kind delim rdr opts pending-forms]
  (let [[start-line start-column] (starting-line-col-info rdr)
        delim (char delim)]
    (binding [*read-delim* true]
      (loop [a (transient [])]
        (let [form (read* rdr false READ_EOF delim opts pending-forms)]
          (if (identical? form READ_FINISHED)
            (persistent! a)
            (if (identical? form READ_EOF)
              (err/throw-eof-delimited rdr kind start-line start-column (count a))
              (recur (conj! a form)))))))))

(defn- read-list
  "Read in a list, including its location if the reader is an indexing reader"
  [rdr _ opts pending-forms]
  (let [[start-line start-column] (starting-line-col-info rdr)
        the-list (read-delimited :list \) rdr opts pending-forms)
        [end-line end-column] (ending-line-col-info rdr)]
    (with-meta (if (empty? the-list)
                 '()
                 (clojure.lang.PersistentList/create the-list))
      (when start-line
        (merge
         (when-let [file (get-file-name rdr)]
           {:file file})
         {:line start-line
          :column start-column
          :end-line end-line
          :end-column end-column})))))

(defn- read-vector
  "Read in a vector, including its location if the reader is an indexing reader"
  [rdr _ opts pending-forms]
  (let [[start-line start-column] (starting-line-col-info rdr)
        the-vector (read-delimited :vector \] rdr opts pending-forms)
        [end-line end-column] (ending-line-col-info rdr)]
    (with-meta the-vector
      (when start-line
        (merge
         (when-let [file (get-file-name rdr)]
           {:file file})
         {:line start-line
          :column start-column
          :end-line end-line
          :end-column end-column})))))

(defn- read-map
  "Read in a map, including its location if the reader is an indexing reader"
  [rdr _ opts pending-forms]
  (let [[start-line start-column] (starting-line-col-info rdr)
        the-map (read-delimited :map \} rdr opts pending-forms)
        map-count (count the-map)
        [end-line end-column] (ending-line-col-info rdr)]
    (when (odd? map-count)
      (err/throw-odd-map rdr start-line start-column the-map))
    (with-meta
      (if (zero? map-count)
        {}
        (RT/map (to-array the-map)))
      (when start-line
        (merge
         (when-let [file (get-file-name rdr)]
           {:file file})
         {:line start-line
          :column start-column
          :end-line end-line
          :end-column end-column})))))

(defn- read-number
  [rdr initch]
  (loop [sb (doto (StringBuilder.) (.Append initch))                                ;;; .append
         ch (read-char rdr)]
    (if (or (whitespace? ch) (macros ch) (nil? ch))
      (let [s (str sb)]
        (unread rdr ch)
        (or (match-number s)
            (err/throw-invalid-number rdr s)))
      (recur (doto sb (.Append ch)) (read-char rdr)))))                             ;;; .append

(defn- escape-char [sb rdr]
  (let [ch (read-char rdr)]
    (case ch
      \t "\t"
      \r "\r"
      \n "\n"
      \\ "\\"
      \" "\""
      \b "\b"
      \f "\f"
      \u (let [ch (read-char rdr)]
           (if (== -1 (char-value-in-radix (int ch) 16))                             ;;; Character/digit 
             (err/throw-invalid-unicode-escape rdr ch)
             (read-unicode-char rdr ch 16 4 true)))
      (if (numeric? ch)
        (let [ch (read-unicode-char rdr ch 8 3 false)]
          (if (> (int ch) 0337)
            (err/throw-bad-octal-number rdr)
            ch))
        (err/throw-bad-escape-char rdr ch)))))

(defn- read-string*
  [reader _ opts pending-forms]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (case ch
      nil (err/throw-eof-reading reader :string sb)
      \\ (recur (doto sb (.Append (escape-char sb reader)))                           ;;; .append
                (read-char reader))
      \" (str sb)
      (recur (doto sb (.Append ch)) (read-char reader)))))                            ;;; .append

(defn- read-symbol
  [rdr initch]
  (let [[line column] (starting-line-col-info rdr)]
    (when-let [token (read-token rdr :symbol initch)]
      (case token

        ;; special symbols
        "nil" nil
        "true" true
        "false" false
        "/" '/
        "NaN" Double/NaN
        "-Infinity" Double/NegativeInfinity                                            ;;; NEGATIVE_INFINITY
        ("Infinity" "+Infinity") Double/PositiveInfinity                               ;;; POSITIVE_INFINITY

        (or (when-let [p (parse-symbol token)]
              (with-meta (symbol (p 0) (p 1))
                (when line
                  (merge
                   (when-let [file (get-file-name rdr)]
                     {:file file})
                   (let [[end-line end-column] (ending-line-col-info rdr)]
                     {:line line
                      :column column
                      :end-line end-line
                      :end-column end-column})))))
            (err/throw-invalid rdr :symbol token))))))

(def ^:dynamic *alias-map*
  "Map from ns alias to ns, if non-nil, it will be used to resolve read-time
   ns aliases instead of (ns-aliases *ns*).

   Defaults to nil"
  nil)

(defn- resolve-alias [sym]
  ((or *alias-map*
       (ns-aliases *ns*)) sym))

(defn- resolve-ns [sym]
  (or ((or *alias-map*
           (ns-aliases *ns*)) sym)
      (find-ns sym)))

(defn- read-keyword
  [reader initch opts pending-forms]
  (let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader :keyword ch)
            s (parse-symbol token)]
        (if s
          (let [^String ns (s 0)
                ^String name (s 1)]
            (if (identical? \: (nth token 0))
              (if ns
                (let [ns (resolve-alias (symbol (subs ns 1)))]
                  (if ns
                    (keyword (str ns) name)
                    (err/throw-invalid reader :keyword (str \: token))))
                (keyword (str *ns*) (subs name 1)))
              (keyword ns name)))
          (err/throw-invalid reader :keyword (str \: token))))
      (err/throw-single-colon reader))))

(defn- wrapping-reader
  "Returns a function which wraps a reader in a call to sym"
  [sym]
  (fn [rdr _ opts pending-forms]
    (list sym (read* rdr true nil opts pending-forms))))

(defn- read-meta
  "Read metadata and return the following object with the metadata applied"
  [rdr _ opts pending-forms]
  (log-source rdr
    (let [[line column] (starting-line-col-info rdr)
          m (desugar-meta (read* rdr true nil opts pending-forms))]  
      (when-not (map? m)
        (err/throw-bad-metadata rdr m))
      (let [o (read* rdr true nil opts pending-forms)]
        (if (instance? IMeta o)
          (let [m (if (and line (seq? o))
                    (assoc m :line line :column column)
                    m)]
            (if (instance? IObj o)
              (with-meta o (merge (meta o) m))
              (reset-meta! o m)))
          (err/throw-bad-metadata-target rdr o))))))

(defn- read-set
  [rdr _ opts pending-forms]
  (let [[start-line start-column] (starting-line-col-info rdr)
        ;; subtract 1 from start-column so it includes the # in the leading #{
        start-column (if start-column (int (dec (int start-column))))
        the-set (PersistentHashSet/createWithCheck
          (read-delimited :set \} rdr opts pending-forms))
        [end-line end-column] (ending-line-col-info rdr)]
    (with-meta the-set
      (when start-line
        (merge
         (when-let [file (get-file-name rdr)]
           {:file file})
         {:line start-line
          :column start-column
          :end-line end-line
          :end-column end-column})))))

(defn- read-discard
  "Read and discard the first object from rdr"
  [rdr _ opts pending-forms]
  (doto rdr
    (read* true nil opts pending-forms)))

(def ^:private RESERVED_FEATURES #{:else :none})

(defn- has-feature?
  [rdr feature opts]
  (if (keyword? feature)
    (or (= :default feature) (contains? (get opts :features) feature))
    (err/throw-feature-not-keyword rdr feature)))

;; WIP, move to errors in the future
(defn- check-eof-error
  [form rdr ^long first-line]
  (when (identical? form READ_EOF)
    (err/throw-eof-error rdr (and (< first-line 0) first-line))))

(defn- check-reserved-features
  [rdr form]
  (when (get RESERVED_FEATURES form)
    (err/reader-error rdr "Feature name " form " is reserved")))

(defn- check-invalid-read-cond
  [form rdr ^long first-line]
  (when (identical? form READ_FINISHED)
    (if (< first-line 0)
      (err/reader-error rdr "read-cond requires an even number of forms")
      (err/reader-error rdr "read-cond starting on line " first-line " requires an even number of forms"))))

(defn- read-suppress
  "Read next form and suppress. Return nil or READ_FINISHED."
  [first-line rdr opts pending-forms]
  (binding [*suppress-read* true]
    (let [form (read* rdr false READ_EOF \) opts pending-forms)]
      (check-eof-error form rdr first-line)
      (when (identical? form READ_FINISHED)
        READ_FINISHED))))

(def ^:private NO_MATCH (Object.))

(defn- match-feature
  "Read next feature. If matched, read next form and return.
   Otherwise, read and skip next form, returning READ_FINISHED or nil."
  [first-line rdr opts pending-forms]
  (let [feature (read* rdr false READ_EOF \) opts pending-forms)]
    (check-eof-error feature rdr first-line)
    (if (= feature READ_FINISHED)
      READ_FINISHED
      (do
        (check-reserved-features rdr feature)
        (if (has-feature? rdr feature opts)
          ;; feature matched, read selected form
          (doto (read* rdr false READ_EOF \) opts pending-forms)
            (check-eof-error rdr first-line)
            (check-invalid-read-cond rdr first-line))
          ;; feature not matched, ignore next form
          (or (read-suppress first-line rdr opts pending-forms)
              NO_MATCH))))))

(defn- read-cond-delimited
  [rdr splicing opts pending-forms]
  (let [first-line (if (indexing-reader? rdr) (get-line-number rdr) -1)
        result (loop [matched NO_MATCH
                      finished nil]
                 (cond
                  ;; still looking for match, read feature+form
                  (identical? matched NO_MATCH)
                  (let [match (match-feature first-line rdr opts pending-forms)]
                    (if (identical? match READ_FINISHED)
                      READ_FINISHED
                      (recur match nil)))

                  ;; found match, just read and ignore the rest
                  (not (identical? finished READ_FINISHED))
                  (recur matched (read-suppress first-line rdr opts pending-forms))

                  :else
                  matched))]
    (if (identical? result READ_FINISHED)
      rdr
      (if splicing
        (if (instance? |System.Collections.Generic.IList`1[System.Object]| result)                    ;;; List
          (do
            (add-front pending-forms result)                                                          ;;; (.addAll ^List pending-forms 0 ^List result)
            rdr)
          (err/reader-error rdr "Spliced form list in read-cond-splicing must implement java.util.List."))
        result))))

(defn- read-cond
  [rdr _ opts pending-forms]
  (when (not (and opts (#{:allow :preserve} (:read-cond opts))))
    (throw (Exception. "Conditional read not allowed")))                                       ;;; RuntimeException.
  (if-let [ch (read-char rdr)]
    (let [splicing (= ch \@)
          ch (if splicing (read-char rdr) ch)]
      (when splicing
        (when-not *read-delim*
          (err/reader-error rdr "cond-splice not in list")))
      (if-let [ch (if (whitespace? ch) (read-past whitespace? rdr) ch)]
        (if (not= ch \()
          (throw (Exception. "read-cond body must be a list"))                                 ;;; RuntimeException.
          (binding [*suppress-read* (or *suppress-read* (= :preserve (:read-cond opts)))]
            (if *suppress-read*
              (reader-conditional (read-list rdr ch opts pending-forms) splicing)
              (read-cond-delimited rdr splicing opts pending-forms))))
        (err/throw-eof-in-character rdr)))
    (err/throw-eof-in-character rdr)))

(def ^:private ^:dynamic arg-env)

(defn- garg
  "Get a symbol for an anonymous ?argument?"
  [^long n]
  (symbol (str (if (== -1 n) "rest" (str "p" n))
               "__" (RT/nextID) "#")))

(defn- read-fn
  [rdr _ opts pending-forms]
  (if (thread-bound? #'arg-env)
    (throw (InvalidOperationException. "Nested #()s are not allowed")))                                    ;;; IllegalStateException
  (binding [arg-env (sorted-map)]
    (let [form (read* (doto rdr (unread \()) true nil opts pending-forms) ;; this sets bindings
          rargs (rseq arg-env)
          args (if rargs
                 (let [higharg (long (key ( first rargs)))]
                   (let [args (loop [i 1 args (transient [])]
                                (if (> i higharg)
                                  (persistent! args)
                                  (recur (inc i) (conj! args (or (get arg-env i)
                                                                 (garg i))))))
                         args (if (arg-env -1)
                                (conj args '& (arg-env -1))
                                args)]
                     args))
                 [])]
      (list 'fn* args form))))

(defn- register-arg
  "Registers an argument to the arg-env"
  [n]
  (if (thread-bound? #'arg-env)
    (if-let [ret (arg-env n)]
      ret
      (let [g (garg n)]
        (set! arg-env (assoc arg-env n g))
        g))
    (throw (InvalidOperationException. "Arg literal not in #()")))) ;; should never hit this               ;;; IllegalStateException

(declare read-symbol)

(defn- read-arg
  [rdr pct opts pending-forms]
  (if-not (thread-bound? #'arg-env)
    (read-symbol rdr pct)
    (let [ch (peek-char rdr)]
      (cond
       (or (whitespace? ch)
           (macro-terminating? ch)
           (nil? ch))
       (register-arg 1)

       (identical? ch \&)
       (do (read-char rdr)
           (register-arg -1))

       :else
       (let [n (read* rdr true nil opts pending-forms)]
         (if-not (integer? n)
           (throw (InvalidOperationException. "Arg literal must be %, %& or %integer"))                     ;;; IllegalStateException
           (register-arg n)))))))

(defn- read-eval
  "Evaluate a reader literal"
  [rdr _ opts pending-forms]
  (when-not *read-eval*
    (err/reader-error rdr "#= not allowed when *read-eval* is false"))
  (eval (read* rdr true nil opts pending-forms)))

(def ^:private ^:dynamic gensym-env nil)

(defn- read-unquote
  [rdr comma opts pending-forms]
  (if-let [ch (peek-char rdr)]
    (if (identical? \@ ch)
      ((wrapping-reader 'clojure.core/unquote-splicing) (doto rdr read-char) \@ opts pending-forms)
      ((wrapping-reader 'clojure.core/unquote) rdr \~ opts pending-forms))))

(declare syntax-quote*)
(defn- unquote-splicing? [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote-splicing)))

(defn- unquote? [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote)))

(defn- expand-list
  "Expand a list by resolving its syntax quotes and unquotes"
  [s]
  (loop [s (seq s) r (transient [])]
    (if s
      (let [item (first s)
            ret (conj! r
                       (cond
                        (unquote? item)          (list 'clojure.core/list (second item))
                        (unquote-splicing? item) (second item)
                        :else                    (list 'clojure.core/list (syntax-quote* item))))]
        (recur (next s) ret))
      (seq (persistent! r)))))

(defn- flatten-map
  "Flatten a map into a seq of alternate keys and values"
  [form]
  (loop [s (seq form) key-vals (transient [])]
    (if s
      (let [e (first s)]
        (recur (next s) (-> key-vals
                          (conj! (key e))
                          (conj! (val e)))))
      (seq (persistent! key-vals)))))

(defn- register-gensym [sym]
  (if-not gensym-env
    (throw (InvalidOperationException. "Gensym literal not in syntax-quote")))                          ;;; IllegalStateException
  (or (get gensym-env sym)
      (let [gs (symbol (str (subs (name sym)
                                  0 (dec (count (name sym))))
                            "__" (RT/nextID) "__auto__"))]
        (set! gensym-env (assoc gensym-env sym gs))
        gs)))

(defn ^:dynamic resolve-symbol
  "Resolve a symbol s into its fully qualified namespace version"
  [s]
  (if (pos? (.IndexOf (name s) "."))                                                                     ;;; .indexOf
    (if (.EndsWith (name s) ".")                                                                         ;;; .endsWith
      (let [csym (symbol (subs (name s) 0 (dec (count (name s)))))]
        (symbol (str (name (resolve-symbol csym)) ".")))
      s)
    (if-let [ns-str (namespace s)]
      (let [ns (resolve-ns (symbol ns-str))]
        (if (or (nil? ns)
                (= (ns-name* ns) ns-str)) ;; not an alias
          s
          (symbol (ns-name* ns) (name s))))
      (if-let [o ((ns-map *ns*) s)]
        (if (class? o)
          (symbol (clojure.lang.Util/NameForType o))                                                     ;;; (.getName ^Class o)  -- need NameForType to handle nastiness
          (if (var? o)
            (symbol (-> ^Var o .ns ns-name*) (-> ^Var o .sym name))))
        (symbol (ns-name* *ns*) (name s))))))

(defn- add-meta [form ret]
  (if (and (instance? IObj form)
           (seq (dissoc (meta form) :line :column :end-line :end-column :file :source)))
    (list 'clojure.core/with-meta ret (syntax-quote* (meta form)))
    ret))

(defn- syntax-quote-coll [type coll]
  ;; We use sequence rather than seq here to fix http://dev.clojure.org/jira/browse/CLJ-1444
  ;; But because of http://dev.clojure.org/jira/browse/CLJ-1586 we still need to call seq on the form
  (let [res (list 'clojure.core/sequence
                  (list 'clojure.core/seq
                        (cons 'clojure.core/concat
                              (expand-list coll))))]
    (if type
      (list 'clojure.core/apply type res)
      res)))

(defn map-func
  "Decide which map type to use, array-map if less than 16 elements"
  [coll]
  (if (>= (count coll) 16)
    'clojure.core/hash-map
    'clojure.core/array-map))

(defn- syntax-quote* [form]
  (->>
   (cond
    (special-symbol? form) (list 'quote form)

    (symbol? form)
    (list 'quote
          (if (namespace form)
            (let [maybe-class ((ns-map *ns*)
                               (symbol (namespace form)))]
              (if (class? maybe-class)
                (symbol (.Name ^Type maybe-class) (name form))               ;;; .getName ^Class
                (resolve-symbol form)))
            (let [sym (str form)]
              (cond
               (.EndsWith sym "#")                                            ;;; .endsWith
               (register-gensym form)

               (.StartsWith sym ".")                                          ;;; .startsWith
               form

               :else (resolve-symbol form)))))

    (unquote? form) (second form)
    (unquote-splicing? form) (throw (InvalidOperationException. "unquote-splice not in list"))                      ;;; IllegalStateException.

    (coll? form)
    (cond

     (instance? IRecord form) form
     (map? form) (syntax-quote-coll (map-func form) (flatten-map form))
     (vector? form) (list 'clojure.core/vec (syntax-quote-coll nil form))
     (set? form) (syntax-quote-coll 'clojure.core/hash-set form)
     (or (seq? form) (list? form))
     (let [seq (seq form)]
       (if seq
         (syntax-quote-coll nil seq)
         '(clojure.core/list)))

     :else (throw (InvalidOperationException. "Unknown Collection type")))                  ;;; UnsupportedOperationException.

    (or (keyword? form)
        (number? form)
        (char? form)
        (string? form)
        (nil? form)
        (instance? Boolean form)
        (instance? Regex form))                                                          ;;; Pattern
    form

    :else (list 'quote form))
   (add-meta form)))

(defn- read-syntax-quote
  [rdr backquote opts pending-forms]
  (binding [gensym-env {}]
    (-> (read* rdr true nil opts pending-forms)
      syntax-quote*)))

(defn- read-namespaced-map
  [rdr _ opts pending-forms]
  (let [token (read-token rdr :namespaced-map (read-char rdr))]
    (if-let [ns (cond
                  (= token ":")
                  (ns-name *ns*)

                  (= \: (first token))
                  (some-> token (subs 1) parse-symbol second' symbol resolve-ns ns-name)

                  :else
                  (some-> token parse-symbol second'))]

      (let [ch (read-past whitespace? rdr)]
        (if (identical? ch \{)
          (let [items (read-delimited :namespaced-map \} rdr opts pending-forms)]
            (when (odd? (count items))
              (err/throw-odd-map rdr nil nil items))
            (let [keys (take-nth 2 items)
                  vals (take-nth 2 (rest items))]

              (RT/map (to-array (mapcat list (namespace-keys (str ns) keys) vals)))))
          (err/throw-ns-map-no-map rdr token)))
      (err/throw-bad-ns rdr token))))

(defn- macros [ch]
  (case ch
    \" read-string*
    \: read-keyword
    \; read-comment
    \' (wrapping-reader 'quote)
    \@ (wrapping-reader 'clojure.core/deref)
    \^ read-meta
    \` read-syntax-quote ;;(wrapping-reader 'syntax-quote)
    \~ read-unquote
    \( read-list
    \) read-unmatched-delimiter
    \[ read-vector
    \] read-unmatched-delimiter
    \{ read-map
    \} read-unmatched-delimiter
    \\ read-char*
    \% read-arg
    \# read-dispatch
    nil))

(defn- dispatch-macros [ch]
  (case ch
    \^ read-meta                ;deprecated
    \' (wrapping-reader 'var)
    \( read-fn
    \= read-eval
    \{ read-set
    \< (throwing-reader "Unreadable form")
    \" read-regex
    \! read-comment
    \_ read-discard
    \? read-cond
    \: read-namespaced-map
    nil))

(defn- read-ctor [rdr class-name opts pending-forms]
  (when-not *read-eval*
    (err/reader-error rdr "Record construction syntax can only be used when *read-eval* == true"))
  (let [class (RT/classForName (name class-name))                                              ;;; (Class/forName (name class-name) false (RT/baseLoader)
        ch (read-past whitespace? rdr)] ;; differs from clojure
    (if-let [[end-ch form] (case ch
                             \[ [\] :short]
                             \{ [\} :extended]
                             nil)]
      (let [entries (to-array (read-delimited :record-ctor end-ch rdr opts pending-forms))
            numargs (count entries)
            all-ctors (.GetConstructors class)
            ctors-num (count all-ctors)]                                                                 ;;; .getConstructors
        (case form
          :short
          (loop [i 0]
            (if (>= i ctors-num)
              (err/reader-error rdr "Unexpected number of constructor arguments to " (str class)
                            ": got" numargs)
              (if (== (count (.GetParameters ^System.Reflection.ConstructorInfo (aget all-ctors i)))     ;;; .getParameterTypes   ^Constructor
                      numargs)
                (Reflector/InvokeConstructor class entries)                                              ;;; invokeConstructor
                (recur (inc i)))))
          :extended
          (let [vals (RT/map entries)]
            (loop [s (keys vals)]
              (if s
                (if-not (keyword? (first s))
                  (err/reader-error rdr "Unreadable ctor form: key must be of type clojure.lang.Keyword")
                  (recur (next s)))))
            (Reflector/InvokeStaticMethod class "create" (object-array [vals])))))                       ;;; invokeStaticMethod
      (err/reader-error rdr "Invalid reader constructor form"))))

(defn- read-tagged [rdr initch opts pending-forms]
  (let [tag (read* rdr true nil opts pending-forms)]
    (if-not (symbol? tag)
      (err/throw-bad-reader-tag rdr tag))
    (if *suppress-read*
      (tagged-literal tag (read* rdr true nil opts pending-forms))
      (if-let [f (or (*data-readers* tag)
                     (default-data-readers tag))]
        (f (read* rdr true nil opts pending-forms))
        (if (.Contains (name tag) ".")                                                                        ;;; .contains 
          (read-ctor rdr tag opts pending-forms)
          (if-let [f *default-data-reader-fn*]
            (f tag (read* rdr true nil opts pending-forms))
            (err/throw-unknown-reader-tag rdr tag)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *read-eval*
  "Defaults to true.

   ***WARNING***
   This setting implies that the full power of the reader is in play,
   including syntax that can cause code to execute. It should never be
   used with untrusted sources. See also: clojure.tools.reader.edn/read.

   When set to logical false in the thread-local binding,
   the eval reader (#=) and *record/type literal syntax* are disabled in read/load.
   Example (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))

   When set to :unknown all reads will fail in contexts where *read-eval*
   has not been explicitly bound to either true or false. This setting
   can be a useful diagnostic tool to ensure that all of your reads
   occur in considered contexts."
  true)

(def ^:dynamic *data-readers*
  "Map from reader tag symbols to data reader Vars.
   Reader tags without namespace qualifiers are reserved for Clojure.
   Default reader tags are defined in clojure.tools.reader/default-data-readers
   and may be overridden by binding this Var."
  {})

(def ^:dynamic *default-data-reader-fn*
  "When no data reader is found for a tag and *default-data-reader-fn*
   is non-nil, it will be called with two arguments, the tag and the value.
   If *default-data-reader-fn* is nil (the default value), an exception
   will be thrown for the unknown tag."
  nil)

(def ^:dynamic *suppress-read* false)

(def default-data-readers
  "Default map of data reader functions provided by Clojure.
   May be overridden by binding *data-readers*"
  {'inst #'data-readers/read-instant-datetime                                            ;;; read-instant-date
   'uuid #'data-readers/default-uuid-reader})

(defn ^:private read*
  ([reader eof-error? sentinel opts pending-forms]
     (read* reader eof-error? sentinel nil opts pending-forms))
  ([reader eof-error? sentinel return-on opts pending-forms]
     (when (= :unknown *read-eval*)
       (err/reader-error "Reading disallowed - *read-eval* bound to :unknown"))
     (try
       (loop []
         (log-source reader
           (if (seq pending-forms)
             (let [pf ^|System.Collections.Generic.LinkedList`1[System.Object]| pending-forms val (-> pf .First .Value)] (.RemoveFirst pf) val)      ;;; (.remove ^List  pending-forms 0)
             (let [ch (read-char reader)]
               (cond
                (whitespace? ch) (recur)
                (nil? ch) (if eof-error? (err/throw-eof-error reader nil) sentinel)
                (= ch return-on) READ_FINISHED
                (number-literal? reader ch) (read-number reader ch)
                :else (let [f (macros ch)]
                        (if f
                          (let [res (f reader ch opts pending-forms)]
                            (if (identical? res reader)
                              (recur)
                              res))
                          (read-symbol reader ch))))))))
       (catch Exception e
         (if (ex-info? e)
           (let [d (ex-data e)]
             (if (= :reader-exception (:type d))
               (throw e)
               (throw (ex-info (.Message e)                                        ;;; .getMessage
                               (merge {:type :reader-exception}
                                      d
                                      (if (indexing-reader? reader)
                                        {:line   (get-line-number reader)
                                         :column (get-column-number reader)
                                         :file   (get-file-name reader)}))
                               e))))
           (throw (ex-info (.Message e)                                            ;;; .getMessage
                           (merge {:type :reader-exception}
                                  (if (indexing-reader? reader)
                                    {:line   (get-line-number reader)
                                     :column (get-column-number reader)
                                     :file   (get-file-name reader)}))
                           e)))))))

(defn read
  "Reads the first object from an IPushbackReader or a java.io.PushbackReader.
   Returns the object read. If EOF, throws if eof-error? is true.
   Otherwise returns sentinel. If no stream is providen, *in* will be used.

   Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals
    :eof - on eof, return value unless :eofthrow, then throw.
           if not specified, will throw

   ***WARNING***
   Note that read can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read

   Note that the function signature of clojure.tools.reader/read and
   clojure.tools.reader.edn/read is not the same for eof-handling"
  {:arglists '([] [reader] [opts reader] [reader eof-error? eof-value])}
  ([] (read *in* true nil))
  ([reader] (read reader true nil))
  ([{eof :eof :as opts :or {eof :eofthrow}} reader] (read* reader (= eof :eofthrow) eof nil opts (|System.Collections.Generic.LinkedList`1[System.Object]|.)))       ;;;  LinkedList.
  ([reader eof-error? sentinel] (read* reader eof-error? sentinel nil {} (|System.Collections.Generic.LinkedList`1[System.Object]|.))))                              ;;; LinkedList.

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty.

   ***WARNING***
   Note that read-string can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read-string

   Note that the function signature of clojure.tools.reader/read-string and
   clojure.tools.reader.edn/read-string is not the same for eof-handling"
  ([s]
     (read-string {} s))
  ([opts s]
     (when (and s (not (identical? s "")))
       (read opts (string-push-back-reader s)))))

(defmacro syntax-quote
  "Macro equivalent to the syntax-quote reader macro (`)."
  [form]
  (binding [gensym-env {}]
    (syntax-quote* form)))
